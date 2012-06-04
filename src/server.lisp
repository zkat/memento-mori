(cl:defpackage #:hipocrite.server
  (:use #:cl #:hipocrite)
  (:export
   ;; API
   #:start
   #:enter-loop
   ;; server protocol
   #:init
   #:on-call
   #:on-cast
   #:on-direct-message
   #:terminate
   ;; Convenience
   #:defcall
   #:defcast))
(cl:in-package #:hipocrite.server)

;;;
;;; Server actor
;;;
(defgeneric init (driver)
  (:method ((driver t)) t))
(defgeneric on-call (driver name args)
  (:method ((driver t) (name t) (args t)) t))
(defgeneric on-cast (driver name args)
  (:method ((driver t) (name t) (args t)) t))
(defgeneric on-direct-message (driver message)
  (:method ((driver t) (message t)) t))
(defgeneric terminate (driver reason)
  (:method ((driver t) (reason t)) t))

(defun enter-loop (driver)
  (init driver)
  (unwind-protect
       (loop for msg = (receive)
          do (cond ((call-msg-p msg)
                    (%handle-call-msg driver msg))
                   ((cast-msg-p msg)
                    (%handle-cast-msg driver msg))
                   (t
                    (on-direct-message driver msg))))
    (terminate driver (make-condition 'actor-exit
                                      :actor (current-actor)
                                      :reason :normal))))

(defun start (driver &key linkp monitorp trap-exits-p
              (name nil namep)
              (debugp *debug-on-error-p*))
  (apply #'spawn
         (lambda () (enter-loop driver))
         :linkp linkp
         :monitorp monitorp
         :trap-exits-p trap-exits-p
         :debugp debugp
         (when namep (list :name name))))

;;;
;;; Call
;;;
(defstruct call-reply monitor values)
(defstruct call-msg name args caller monitor)

(define-condition call-error (error) ())
(define-condition callee-down (call-error) ())
(define-condition call-timeout (call-error) ())

(defun call (actor name args &key (timeout 5))
  (let ((monitor (monitor actor)))
    (send actor (make-call-msg :monitor monitor
                               :caller (current-actor)
                               :name name
                               :args args))
    (receive-cond (reply :timeout timeout :on-timeout (error 'call-timeout))
      ((and (call-reply-p reply)
            (eq (call-reply-monitor reply) monitor))
       (demonitor monitor)
       (values-list (call-reply-values reply)))
      ((and (monitor-exit-p reply)
            (eq monitor (monitor-exit-monitor reply)))
       (error 'callee-down)))))

(defun %handle-call-msg (driver msg)
  (let ((results (multiple-value-list
                  (on-call driver
                           (call-msg-name msg)
                           (call-msg-args msg)))))
    (when results
      (send (call-msg-caller msg)
            (make-call-reply
             :monitor (call-msg-monitor msg)
             :values results)))))

(defmacro defcall (name (server-var
                         server-class
                         &key
                         (define-function-p t)
                         (timeout nil timeoutp))
                   lambda-list &body body)
  (let ((args-var (gensym "ARGS")))
    `(progn
       ,@(when define-function-p
               `((defun ,name (,server-var &rest args)
                   (call ,server-var ',name args ,@(when timeoutp `(:timeout ,timeout))))))
       (defmethod on-call ((,server-var ,server-class)
                           (,(gensym "NAME") (eql ',name))
                           ,args-var)
         (flet ((,name ,lambda-list ,@body))
           (apply #',name ,args-var))))))

;;;
;;; Cast
;;;
(defstruct cast-msg name args)
(defun cast (actor name args)
  (send actor (make-cast-msg :name name :args args)))

(defmacro defcast (name (server-var
                         server-class
                         &key
                         (define-function-p t)
                         (timeout nil timeoutp))
                   lambda-list &body body)
  (let ((args-var (gensym "ARGS")))
    `(progn
       ,@(when define-function-p
               `((defun ,name (,server-var &rest args)
                   (cast ,server-var ',name args ,@(when timeoutp `(:timeout ,timeout))))))
       (defmethod on-cast ((,server-var ,server-class)
                           (,(gensym "NAME") (eql ',name))
                           ,args-var)
         (flet ((,name ,lambda-list ,@body))
           (apply #',name ,args-var))))))

(defun %handle-cast-msg (driver msg)
  (on-cast driver (cast-msg-name msg) (cast-msg-args msg)))
