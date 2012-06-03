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
             (setf exit (run-actor-function actor func debugp))
          (notify-links actor exit)
          (notify-monitors actor exit)
          (when namep (unregister name)))))))

(defun run-actor-function (actor func debugp)
  (handler-bind ((actor-exit (lambda (exit)
                               (return-from run-actor-function exit)))
                 (error (lambda (e)
                          (when debugp (invoke-debugger e))
                          (return-from run-actor-function
                            (make-condition 'actor-exit
                                            :actor actor
                                            :type :error
                                            :info e)))))
    (apply #'make-condition
           'actor-exit
           :actor actor
           (restart-case
               (list :type :normal
                     :info (with-interrupts (funcall func)))
             (kill-actor ()
               (list :type :kill :info :killed))))))

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
(define-condition actor-exit (condition)
  ((actor :initarg :actor :reader actor-exit-actor)
   (type :initarg :type :reader actor-exit-type)
   (info :initarg :info :reader actor-exit-info)))

(defmethod print-object ((actor-exit actor-exit) stream)
  (print-unreadable-object (actor-exit stream :type t :identity t)
    (format stream "[Actor: ~a; Type: ~s]"
            (actor-exit-actor actor-exit)
            (actor-exit-type actor-exit))))

(defun signal-exit (actor exit &optional forcep)
  (if (and (bt:with-lock-held ((actor-exit-lock actor))
             (actor-trap-exits-p actor))
           (not forcep))
      (send actor exit)
      (bt:interrupt-thread (actor-thread actor)
                           (lambda ()
                             (signal exit)))))

(defun exit (reason &optional (actor (current-actor)))
  (signal-exit actor (make-condition 'actor-exit
                                     :actor actor
                                     :type :exit
                                     :info reason)))

(defun kill (&optional (actor (current-actor)))
  (signal-exit actor (make-condition 'actor-exit
                                     :actor actor
                                     :type :kill
                                     :info :killed) t))

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

(defun link (actor &optional (actor2 (current-actor)))
  (bt:with-lock-held (*link-lock*)
    (pushnew actor (actor-links actor2))
    (pushnew actor2 (actor-links actor))))

(defun unlink (actor &optional (actor2 (current-actor)))
  (bt:with-recursive-lock-held (*link-lock*)
    (removef (actor-links actor) actor2)
    (removef (actor-links actor2) actor)))

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
(defstruct monitor-ref monitor monitored-actor)
(defmethod print-object ((monitor-ref monitor-ref) stream)
  (print-unreadable-object (monitor-ref stream :type t :identity t)
    (format stream "Actor: ~a" (monitor-ref-monitored-actor monitor-ref))))

(defun monitor (actor &optional (monitor (current-actor)))
  (bt:with-lock-held ((actor-monitor-lock actor))
    (let ((ref (make-monitor-ref :monitor monitor :monitored-actor actor)))
      (push ref (actor-monitors actor))
      ref)))

(defun demonitor (ref)
  (let ((actor (monitor-ref-monitored-actor ref)))
    (bt:with-lock-held ((actor-monitor-lock actor))
      (removef ref (actor-monitors actor)))
    t))

(defstruct monitored-actor-down actor monitor)
(defun notify-monitors (actor exit)
  (bt:with-lock-held ((actor-monitor-lock actor))
    (loop for ref in (actor-monitors actor)
       ;; TODO - I don't know wtf I really want to return here.
       do (send (monitor-ref-monitor ref) (list :down ref actor exit)))
    (setf (actor-monitors actor) nil)))
