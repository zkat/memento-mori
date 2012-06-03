(in-package #:hipocrite)

(defparameter *link-lock* (bt:make-lock))
(defvar *current-actor* nil)
(defvar +killswitch+ (gensym "BRUTAL-KILL"))
(defvar *registered-actors* (make-hash-table))
(defvar *registration-lock* (bt:make-lock))
(defvar *debug-on-error-p* nil)

(defstruct actor
  (mailbox (hipocrite.mailbox:make-mailbox))
  ;; TODO
  ;; (monitor-lock (bt:make-lock))
  ;; monitors
  name
  named-p
  links
  (exit-lock (bt:make-lock))
  trap-exits-p
  thread
  function)

(defmethod print-object ((actor actor) stream)
  (print-unreadable-object (actor stream :type t :identity t)
    (bt:with-lock-held (*registration-lock*)
      (when (actor-named-p actor)
        (format stream "~s" (actor-name actor))))))

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

;; It's gotta be global, or it'll cause deadlocks. :(
(defun current-actor ()
  *current-actor*)

(defun spawn (func &key
              linkp trap-exits-p
              (name nil namep) (debugp *debug-on-error-p*))
  (let ((actor (make-actor :function func :trap-exits-p trap-exits-p)))
    (when linkp
      (link actor (current-actor)))
    (setf (actor-thread actor)
          (bt:make-thread
           (make-actor-function actor func linkp namep name debugp)
           :initial-bindings
           (list*
            (cons '*current-actor* actor)
            (cons '*debug-on-error-p* *debug-on-error-p*)
            bt:*default-special-bindings*)))
    actor))

(defun link (actor &optional (actor2 (current-actor)))
  (bt:with-lock-held (*link-lock*)
    (push actor (actor-links actor2))
    (push actor2 (actor-links actor))))

(defun unlink (actor &optional (actor2 (current-actor)))
  (bt:with-recursive-lock-held (*link-lock*)
    (removef (actor-links actor) actor2)
    (removef (actor-links actor2) actor)))

(define-condition actor-exit (condition)
  ((actor :initarg :actor :reader actor-exit-actor)
   (reason :initarg :reason :reader actor-exit-reason)))

(defmethod print-object ((actor-exit actor-exit) stream)
  (print-unreadable-object (actor-exit stream :type t :identity t)
    (format stream "[Actor: ~a; Reason: ~s]"
            (actor-exit-actor actor-exit)
            (actor-exit-reason actor-exit))))

(defun signal-exit (actor exit &optional forcep)
  (bt:with-lock-held ((actor-exit-lock actor))
    (if (and (actor-trap-exits-p actor) (not forcep))
        (send actor exit)
        (bt:interrupt-thread (actor-thread actor)
                             (lambda ()
                               (signal exit))))))

(defun exit (reason &optional (actor (current-actor)))
  (signal-exit actor (make-condition 'actor-exit :actor actor :reason reason)))

(defun kill (&optional (actor (current-actor)))
  (signal-exit actor (make-condition 'actor-exit :actor actor :reason :killed) t))

(defmacro without-interrupts (&body body)
  #+sbcl
  `(sb-sys:without-interrupts ,@body)
  #+ccl
  `(ccl:without-interrupts ,@body)
  #-(or sbcl ccl)
  (error "Unsupported"))

(defmacro with-local-interrupts (&body body)
  #+sbcl
  `(sb-sys:with-local-interrupts ,@body)
  #+ccl
  `(ccl:with-interrupts-enabled ,@body)
  #-(or sbcl ccl)
  (error "Unsupported"))
 
(defun make-actor-function (actor func linkp namep name debugp
                            &aux (parent (current-actor)))
  (lambda ()
    (without-interrupts
      (when linkp (link actor parent))
      (when namep (register name actor))
      (let (exit-reason)
        (unwind-protect
             (setf exit-reason
                   (block result
                     (let ((*debugger-hook*
                            (if debugp
                                *debugger-hook*
                                (lambda (e v)
                                  (declare (ignore v))
                                  (return-from result e)))))
                       (handler-case
                           (restart-case
                               (cons :normal
                                     (with-local-interrupts
                                       (funcall func)))
                             (kill-actor ()
                               :killed))
                         (actor-exit (exit) exit)))))
          (bt:with-recursive-lock-held (*link-lock*)
            (when (actor-links actor)
              (loop for linked-actor in (actor-links actor)
                 do (unlink actor linked-actor)
                   (signal-exit linked-actor exit-reason)))))))))

(defun actor-alive-p (actor)
  (bt:thread-alive-p (actor-thread actor)))

(defun send (actor message)
  (hipocrite.mailbox:send (actor-mailbox actor) message))

(defun receive (&key timeout on-timeout)
  (hipocrite.mailbox:receive (actor-mailbox (current-actor))
                             :timeout timeout
                             :on-timeout on-timeout))
