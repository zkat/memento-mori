(defpackage #:memento-mori
  (:use #:cl #:alexandria #:memento-mori.utils)
  (:import-from #:memento-mori.mailbox #:receive-timeout)
  (:nicknames #:mori)
  (:export
   ;; Core
   #:*debug-on-error-p*
   #:enable-crash-logging
   #:disable-crash-logging
   #:crash-logging-enabled-p
   #:current-actor
   #:actor-alive-p
   #:trap-exits-p
   #:enable-trap-exits
   #:disable-trap-exits
   #:spawn
   ;; Messaging
   #:send
   #:receive
   #:receive-cond
   #:flush-messages
   ;; Exits
   #:actor-exit
   #:actor-shutdown
   #:actor-kill
   #:actor-completion
   #:actor-error
   #:actor-exit-reason
   #:exit
   #:shutdown
   #:kill
   #:actor-break
   ;; Named actors
   #:find-actor
   #:ensure-actor
   #:registered-names
   #:register
   #:unregister
   #:no-such-actor
   #:actor-already-exists
   ;; Linking
   #:link
   #:unlink
   #:link-exit
   #:link-exit-p
   #:link-exit-linked-actor
   #:link-exit-type
   #:link-exit-reason
   ;; Monitoring
   #:monitor
   #:demonitor
   #:monitorp
   #:monitor-monitored-actor
   #:monitor-exit
   #:monitor-exit-p
   #:monitor-exit-monitor
   #:monitor-exit-type
   #:monitor-exit-reason))
(in-package #:memento-mori)

;;;
;;; Actors
;;;
(defvar *debug-on-error-p* nil)
(defvar *current-actor* nil)

(defstruct actor
  (mailbox (memento-mori.mailbox:make-mailbox))
  (monitor-lock (bt:make-lock))
  monitors
  name
  named-p
  links
  (exit-lock (bt:make-lock))
  trap-exits-setting
  thread
  function)

(defmethod print-object ((actor actor) stream)
  (print-unreadable-object (actor stream :type t :identity t)
    (maybe-format-actor-name actor stream)
    (format stream "[~a msgs]" (memento-mori.mailbox:mailbox-count
                                (actor-mailbox actor)))))

(defun current-actor ()
  *current-actor*)

(defun actor-alive-p (actor)
  (bt:thread-alive-p (actor-thread (ensure-actor actor))))

(defun %trap-exits-p (actor)
  (bt:with-recursive-lock-held ((actor-exit-lock (ensure-actor actor)))
    (actor-trap-exits-setting actor)))

(defun trap-exits-p (&aux (actor (current-actor)))
  (%trap-exits-p actor))

(defun enable-trap-exits (&aux (actor (current-actor)))
  (bt:with-recursive-lock-held ((actor-exit-lock actor))
    (setf (actor-trap-exits-setting actor) t)))

(defun disable-trap-exits (&aux (actor (current-actor)))
  (bt:with-recursive-lock-held ((actor-exit-lock actor))
    (setf (actor-trap-exits-setting actor) nil)))

(defun spawn (func &key
              linkp monitorp trap-exits-p
              (name nil namep) (debugp *debug-on-error-p*))
  (when namep
    (check-type name symbol "a valid actor name"))
  (let* ((actor (make-actor :function func :trap-exits-setting trap-exits-p))
         (monitor (when monitorp (%monitor actor (current-actor) nil))))
    (when namep (register name actor))
    (setf (actor-thread actor)
          (bt:make-thread
           (make-actor-function actor func linkp namep name debugp)
           :name (format nil "Mori actor thread for ~s" actor)
           :initial-bindings
           (list*
            (cons '*current-actor* actor)
            (cons '*debug-on-error-p* *debug-on-error-p*)
            bt:*default-special-bindings*)))
    (if monitor
        (values actor monitor)
        actor)))

(defvar *log-settings-lock* (bt:make-lock))
(let ((log-crashes-p t))
  (defun enable-crash-logging ()
    (bt:with-recursive-lock-held (*log-settings-lock*)
      (setf log-crashes-p t)))
  (defun disable-crash-logging ()
    (bt:with-recursive-lock-held (*log-settings-lock*)
      (setf log-crashes-p nil)))
  (defun crash-logging-enabled-p ()
    (bt:with-recursive-lock-held (*log-settings-lock*)
      log-crashes-p)))

(defvar *debugger-lock* (bt:make-lock))

(defun make-actor-function (actor func linkp namep name debugp
                            &aux (parent (current-actor)))
  (lambda ()
    (without-interrupts
      (let (exit)
        (unwind-protect
             (setf exit
                   (block run-actor-function
                     (handler-bind ((actor-exit (lambda (exit)
                                                  (return-from run-actor-function exit)))
                                    (error (lambda (e)
                                             (when debugp
                                               (bt:with-recursive-lock-held (*debugger-lock*)
                                                 (invoke-debugger e)))
                                             (return-from run-actor-function
                                               (make-condition 'actor-error
                                                               :reason e)))))
                       (restart-case
                           (make-condition 'actor-completion
                                           :reason
                                           (#+sbcl sb-sys:allow-with-interrupts
                                            #-sbcl progn
                                             (when linkp (link parent))
                                             (with-interrupts (funcall func))))

                         (kill-actor ()
                           (make-condition 'actor-kill))))))
          (when namep (unregister name nil))
          (notify-links actor exit)
          (notify-monitors actor exit)
          (when (crash-logging-enabled-p)
            (log-crash actor exit)))))))

(defun log-crash (actor exit)
  (when-let (pkg (find-package '#:memento-mori.logger))
    (when-let (symbol (find-symbol (string '#:log-crash) pkg))
      (funcall symbol actor exit))))

;;;
;;; Messaging
;;;
(defun send (actor message)
  (memento-mori.mailbox:send (actor-mailbox (ensure-actor actor)) message))

(defun receive (&key timeout on-timeout)
  (memento-mori.mailbox:receive (actor-mailbox (current-actor))
                             :timeout timeout
                             :on-timeout on-timeout))

(defmacro receive-cond ((value-var &key timeout on-timeout) &body clauses)
  `(memento-mori.mailbox:receive-cond (,value-var (actor-mailbox (current-actor))
                                               :timeout ,timeout
                                               :on-timeout ,on-timeout)
     ,@clauses))

(defun flush-messages ()
  (receive :timeout 0 :on-timeout (lambda () (return-from flush-messages t)))
  (flush-messages))

;;;
;;; Exits
;;;

(define-condition actor-exit (condition) ; Actor stopped for unspecified reason
  ((reason :initarg :reason :reader actor-exit-reason)))

(define-condition actor-shutdown (actor-exit) ()) ; A shutdown was requested
(define-condition actor-kill (actor-exit) ((reason :initform :killed))) ; killdeathkill
(define-condition actor-completion (actor-exit) ()) ; Actor function completed normally
(define-condition actor-error (actor-exit) ()) ; Error condition forced actor exit

(defmethod print-object ((actor-exit actor-exit) stream)
  (print-unreadable-object (actor-exit stream :type t :identity t)
    (format stream "reason: ~s" (actor-exit-reason actor-exit))))

(defun signal-exit (actor exit &aux (actor (ensure-actor actor)))
  (cond ((eq actor (current-actor))
         (signal exit))
        ((and (%trap-exits-p actor)
              (not (typep exit 'actor-kill)))
         (send-exit-message actor exit))
        (t
         (bt:interrupt-thread (actor-thread actor)
                              (lambda ()
                                (without-interrupts
                                  (signal exit)))))))

(defun shutdown (reason &optional (actor (current-actor)))
  (signal-exit actor (make-condition 'actor-shutdown
                                     :reason reason)))

(defun exit (reason &optional (actor (current-actor)))
  (signal-exit actor (make-condition 'actor-exit
                                     :reason reason)))

(defun kill (&optional (actor (current-actor)))
  (signal-exit actor (make-condition 'actor-kill)))

(defun actor-break (actor &optional string &rest args)
  (bt:interrupt-thread (actor-thread (ensure-actor actor))
                       (apply #'curry #'break string args)))

;;;
;;; Registration
;;;
(defvar *registered-actors* (make-hash-table :test #'eq))
(defvar *registration-lock* (bt:make-lock))

(define-condition no-such-actor (error)
  ((name :initarg :name :reader no-such-actor-name))
  (:report (lambda (e stream)
             (format stream "~S is not the name of a registered actor."
                     (no-such-actor-name e)))))

(define-condition actor-already-exists (error)
  ((name :initarg :name :reader actor-already-exists-name)
   (existing-actor :initarg :existing :reader actor-already-exists-existing-actor))
  (:report (lambda (e stream)
             (format stream "~S is already registered as ~s."
                     (actor-already-exists-existing-actor e)
                     (actor-already-exists-name e)))))

(defun list-registered-names ()
  (bt:with-recursive-lock-held (*registration-lock*)
    (hash-table-keys *registered-actors*)))

(defun find-actor (name &optional (errorp t))
  (check-type name symbol "a valid actor name")
  (bt:with-recursive-lock-held (*registration-lock*)
    (multiple-value-bind (actor foundp)
        (gethash name *registered-actors*)
      (cond (foundp actor)
            (errorp (error 'no-such-actor :name name))
            (t nil)))))

(defun register (name actor &optional (errorp t))
  (check-type name symbol "a valid actor name")
  (check-type actor actor "an actor object")
  (bt:with-recursive-lock-held (*registration-lock*)
    (when errorp
      (when-let ((old-actor (find-actor name nil)))
        (restart-case
            (error 'actor-already-exists
                   :name name
                   :existing old-actor)
          (replace ()
            :report "Replace the registration. Existing actor will continue to run."
            nil)
          (shutdown-and-replace ()
            :report "Replace the registration. Existing actor will be sent a shutdown request."
            (shutdown "Replaced during registration." old-actor))
          (kill-and-replace ()
            :report "Replace the registration. Existing actor will be killed."
            (kill old-actor)))))
    (setf (actor-name actor) name
          (actor-named-p actor) t
          (gethash name *registered-actors*) actor)))

(defun ensure-actor (actor)
  (etypecase actor
    (actor actor)
    (symbol (find-actor actor))))

(defun unregister (name &optional (errorp t))
  (check-type name symbol "a valid actor name")
  (bt:with-recursive-lock-held (*registration-lock*)
    (when (and (null (remhash name *registered-actors*))
               errorp)
      (error 'no-such-actor :name name))))

(defun maybe-format-actor-name (actor stream)
  (bt:with-recursive-lock-held (*registration-lock*)
    (when (actor-named-p actor)
      (format stream "~s " (actor-name actor)))))

;;;
;;; Linking
;;;
(defvar *link-lock* (bt:make-lock))

(defstruct link-exit linked-actor type reason)
(defmethod print-object ((link-exit link-exit) stream)
  (print-unreadable-object (link-exit stream :type t :identity t)
    (format stream "[~A ~S]"
            (link-exit-type link-exit)
            (link-exit-reason link-exit))))

(defun link (actor &aux (self (current-actor)))
  (let ((actor (ensure-actor actor))
        (self (ensure-actor self)))
    (bt:with-recursive-lock-held (*link-lock*)
      (assert (actor-alive-p actor) ()
              "Cannot link to a dead actor.")
      (pushnew actor (actor-links self))
      (pushnew self (actor-links actor)))))

(defun unlink (actor &aux (self (current-actor)))
  (let ((actor (ensure-actor actor))
        (self (ensure-actor self)))
    (bt:with-recursive-lock-held (*link-lock*)
      (assert (actor-alive-p actor)
              ()
              "Cannot unlink from a dead actor.")
      (removef (actor-links actor) self)
      (removef (actor-links self) actor))))

(defun send-exit-message (actor exit)
  (send actor (make-link-exit
               :linked-actor (current-actor)
               :type (type-of exit)
               :reason (actor-exit-reason exit))))

(defun notify-links (actor exit)
  (bt:with-recursive-lock-held (*link-lock*)
    (when (actor-links actor)
      (loop for linked-actor in (actor-links actor)
         do
         (signal-exit linked-actor exit)
         (removef (actor-links linked-actor) actor)))
    (setf (actor-links actor) nil)))

;;;
;;; Monitors
;;;
(defstruct (monitor (:predicate monitorp)) observer monitored-actor)
(defmethod print-object ((monitor monitor) stream)
  (print-unreadable-object (monitor stream :type t :identity t)
    (format stream "Actor: ~a" (monitor-monitored-actor monitor))))

(defun %monitor (actor observer confirm-alive)
  (let ((actor (ensure-actor actor))
        (observer (ensure-actor observer)))
    (bt:with-recursive-lock-held ((actor-monitor-lock actor))
      (when confirm-alive
        (assert (actor-alive-p actor) () "Cannot monitor a dead actor."))
      (let ((ref (make-monitor :observer observer :monitored-actor actor)))
        (push ref (actor-monitors actor))
        ref))))

(defun monitor (actor &aux (self (current-actor)))
  (%monitor actor self t))

(defun demonitor (ref)
  (let ((actor (monitor-monitored-actor ref)))
    (bt:with-recursive-lock-held ((actor-monitor-lock actor))
      (removef (actor-monitors actor) ref))
    t))

(defstruct monitor-exit monitor actor type reason)
(defmethod print-object ((monitor-exit monitor-exit) stream)
  (print-unreadable-object (monitor-exit stream :type t :identity t)
    (format stream "[~A ~S]"
            (monitor-exit-type monitor-exit)
            (monitor-exit-reason monitor-exit))))

(defun notify-monitors (actor exit)
  (bt:with-recursive-lock-held ((actor-monitor-lock actor))
    (loop for monitor in (actor-monitors actor)
       do (send (monitor-observer monitor)
                (make-monitor-exit :monitor monitor
                                   :actor actor
                                   :type (type-of exit)
                                   :reason (actor-exit-reason exit))))
    (setf (actor-monitors actor) nil)))
