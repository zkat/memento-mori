(defpackage #:hipocrite
  (:use #:cl #:alexandria #:hipocrite.utils)
  (:export
   ;; Core
   #:*debug-on-error-p*
   #:current-actor
   #:actor-alive-p
   #:spawn
   ;; Messaging
   #:send
   #:receive
   #:receive-cond
   #:flush-messages
   ;; Exits
   #:actor-exit
   #:actor-kill
   #:actor-completion
   #:actor-error
   #:exit
   #:kill
   ;; Named actors
   #:find-actor
   #:register
   #:unregister
   ;; Linking
   #:link
   #:unlink
   #:link-exit-p
   #:link-exit-type
   #:link-exit-reason
   ;; Monitoring
   #:monitor
   #:demonitor
   #:monitorp
   #:monitor-monitored-actor
   #:monitor-exit-p
   #:monitor-exit-monitor
   #:monitor-exit-type
   #:monitor-exit-reason))
(in-package #:hipocrite)

;;;
;;; Actors
;;;
(defvar *debug-on-error-p* nil)
(defvar *current-actor* nil)

(defstruct actor
  (mailbox (hipocrite.mailbox:make-mailbox))
  (monitor-lock (bt:make-lock))
  monitors
  name
  named-p
  links
  (exit-lock (bt:make-lock))
  trap-exits-p
  thread
  function)

(defmethod print-object ((actor actor) stream)
  (print-unreadable-object (actor stream :type t :identity t)
    (maybe-format-actor-name actor stream)
    (format stream "[~a msgs]" (hipocrite.mailbox:mailbox-count
                           (actor-mailbox actor)))))

(defun current-actor ()
  *current-actor*)

(defun actor-alive-p (actor)
  (bt:thread-alive-p (actor-thread actor)))

(defun spawn (func &key
              linkp monitorp trap-exits-p
              (name nil namep) (debugp *debug-on-error-p*))
  (let* ((actor (make-actor :function func :trap-exits-p trap-exits-p))
         (monitor (when monitorp (monitor actor))))
    (setf (actor-thread actor)
          (bt:make-thread
           (make-actor-function actor func linkp namep name debugp)
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

(defun run-actor-function (func debugp)
  (handler-bind ((actor-stop (lambda (exit)
                               (return-from run-actor-function exit)))
                 (error (lambda (e)
                          (when debugp (invoke-debugger e))
                          (return-from run-actor-function
                            (make-condition 'actor-error
                                            :reason e)))))
    (restart-case
        (make-condition 'actor-completion
                        :reason
                        (with-interrupts (funcall func)))

      (kill-actor ()
        (make-condition 'actor-kill
                        :reason "Killed in restart")))))

;;;
;;; Messaging
;;;
(defun send (actor message)
  (hipocrite.mailbox:send (actor-mailbox actor) message))

(defun receive (&key timeout on-timeout)
  (hipocrite.mailbox:receive (actor-mailbox (current-actor))
                             :timeout timeout
                             :on-timeout on-timeout))

(defmacro receive-cond ((value-var &key timeout on-timeout) &body clauses)
  `(hipocrite.mailbox:receive-cond (,value-var (actor-mailbox (current-actor))
                                               :timeout ,timeout
                                               :on-timeout ,on-timeout)
     ,@clauses))

(defun flush-messages ()
  (unless (nth-value 1 (receive :timeout 0))
    (flush-messages)))

;;;
;;; Exits
;;;

(define-condition actor-stop (condition)
  ((reason :initarg :reason :reader actor-stop-reason)))

(define-condition actor-exit (actor-stop) ())
(define-condition actor-kill (actor-stop) ())
(define-condition actor-completion (actor-stop) ())
(define-condition actor-error (actor-stop) ())

(defmethod print-object ((actor-stop actor-stop) stream)
  (print-unreadable-object (actor-stop stream :type t :identity t)
    (format stream "reason: ~s" (actor-stop-reason actor-stop))))

(defun signal-exit (actor exit)
  (if (and (bt:with-lock-held ((actor-exit-lock actor))
             (actor-trap-exits-p actor))
           (not (typep exit 'actor-kill)))
      (send-exit-message actor exit)
      (bt:interrupt-thread (actor-thread actor)
                           (lambda ()
                             (signal exit)))))

(defun exit (reason &optional (actor (current-actor)))
  (signal-exit actor (make-condition 'actor-exit
                                     :reason reason)))

(defun kill (reason &optional (actor (current-actor)))
  (signal-exit actor (make-condition 'actor-kill
                                     :reason reason)))

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
      (format stream "~s" (actor-name actor)))))

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
               :reason (actor-stop-reason exit))))

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
                                   :reason (actor-stop-reason exit))))
    (setf (actor-monitors actor) nil)))
