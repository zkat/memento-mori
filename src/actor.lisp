(defpackage #:memento-mori
  (:use #:cl #:alexandria #:memento-mori.utils)
  (:import-from #:memento-mori.mailbox #:receive-timeout)
  (:nicknames #:mori)
  (:export
   ;; Core
   #:*debug-on-error-p*
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
   #:exit
   #:kill
   #:actor-break
   ;; Named actors
   #:find-actor
   #:register
   #:unregister
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
  (bt:thread-alive-p (actor-thread actor)))

(defun %trap-exits-p (actor)
  (bt:with-lock-held ((actor-exit-lock actor))
    (actor-trap-exits-setting actor)))

(defun trap-exits-p (&aux (actor (current-actor)))
  (%trap-exits-p actor))

(defun enable-trap-exits (&aux (actor (current-actor)))
  (bt:with-lock-held ((actor-exit-lock actor))
    (setf (actor-trap-exits-setting actor) t)))

(defun disable-trap-exits (&aux (actor (current-actor)))
  (bt:with-lock-held ((actor-exit-lock actor))
    (setf (actor-trap-exits-setting actor) nil)))

(defun spawn (func &key
              linkp monitorp trap-exits-p
              (name nil namep) (debugp *debug-on-error-p*))
  (let* ((actor (make-actor :function func :trap-exits-setting trap-exits-p))
         (monitor (when monitorp (monitor actor))))
    (setf (actor-thread actor)
          (bt:make-thread
           (make-actor-function actor func linkp namep name debugp)
           :name (format nil "Mori actor thread for ~s" actor)
           :initial-bindings
           (list*
            (cons '*current-actor* actor)
            (cons '*debug-on-error-p* *debug-on-error-p*)
            bt:*default-special-bindings*)))
    (values actor monitor)))

(defun make-actor-function (actor func linkp namep name debugp
                            &aux (parent (current-actor)))
  (lambda ()
    (without-interrupts
      (when linkp (link actor parent))
      (when namep (register name actor))
      (let (exit)
        (unwind-protect
             (setf exit (run-actor-function func debugp))
          (notify-links actor exit)
          (notify-monitors actor exit)
          (when namep (unregister name)))))))

(defvar *debugger-lock* (bt:make-lock))

(defun run-actor-function (func debugp)
  (handler-bind ((actor-exit (lambda (exit)
                               (return-from run-actor-function exit)))
                 (error (lambda (e)
                          (when debugp
                            (bt:with-lock-held (*debugger-lock*)
                              (invoke-debugger e)))
                          (return-from run-actor-function
                            (make-condition 'actor-error
                                            :reason e)))))
    (restart-case
        (make-condition 'actor-completion
                        :reason
                        (with-interrupts (funcall func)))

      (kill-actor ()
        (make-condition 'actor-kill)))))

;;;
;;; Messaging
;;;
(defun send (actor message)
  (memento-mori.mailbox:send (actor-mailbox actor) message))

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

(defun signal-exit (actor exit)
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

(defun exit (reason &optional (actor (current-actor)))
  (signal-exit actor (make-condition 'actor-exit
                                     :reason reason)))

(defun kill (&optional (actor (current-actor)))
  (signal-exit actor (make-condition 'actor-kill)))

(defun actor-break (actor &optional string &rest args)
  (bt:interrupt-thread (actor-thread actor)
                       (apply #'curry #'break string args)))

;;;
;;; Registration
;;;
(defvar *registered-actors* (make-hash-table))
(defvar *registration-lock* (bt:make-lock))

(defun find-actor (name)
  (bt:with-lock-held (*registration-lock*)
    (values (gethash name *registered-actors*))))

(defun register (name actor)
  (bt:with-lock-held (*registration-lock*)
    (setf (actor-name actor) name
          (actor-named-p actor) t
          (gethash name *registered-actors*) actor)))

(defun unregister (name)
  (bt:with-lock-held (*registration-lock*)
    (remhash name *registered-actors*)))

(defun maybe-format-actor-name (actor stream)
  (bt:with-lock-held (*registration-lock*)
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

(defun link (actor &optional (actor2 (current-actor)))
  (bt:with-lock-held (*link-lock*)
    (pushnew actor (actor-links actor2))
    (pushnew actor2 (actor-links actor))))

(defun unlink (actor &optional (actor2 (current-actor)))
  (bt:with-recursive-lock-held (*link-lock*)
    (removef (actor-links actor) actor2)
    (removef (actor-links actor2) actor)))

(defun send-exit-message (actor exit)
  (send actor (make-link-exit
               :linked-actor (current-actor)
               :type (type-of exit)
               :reason (actor-exit-reason exit))))

(defun notify-links (actor exit)
  (bt:with-lock-held (*link-lock*)
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

(defun monitor (actor &optional (observer (current-actor)))
  (bt:with-lock-held ((actor-monitor-lock actor))
    (let ((ref (make-monitor :observer observer :monitored-actor actor)))
      (push ref (actor-monitors actor))
      ref)))

(defun demonitor (ref)
  (let ((actor (monitor-monitored-actor ref)))
    (bt:with-lock-held ((actor-monitor-lock actor))
      (removef (actor-monitors actor) ref))
    t))

(defstruct monitor-exit monitor actor type reason)
(defmethod print-object ((monitor-exit monitor-exit) stream)
  (print-unreadable-object (monitor-exit stream :type t :identity t)
    (format stream "[~A ~S]"
            (monitor-exit-type monitor-exit)
            (monitor-exit-reason monitor-exit))))

(defun notify-monitors (actor exit)
  (bt:with-lock-held ((actor-monitor-lock actor))
    (loop for monitor in (actor-monitors actor)
       do (send (monitor-observer monitor)
                (make-monitor-exit :monitor monitor
                                   :actor actor
                                   :type (type-of exit)
                                   :reason (actor-exit-reason exit))))
    (setf (actor-monitors actor) nil)))
