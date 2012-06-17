(cl:defpackage #:memento-mori.server
  (:use #:cl #:alexandria #:memento-mori)
  (:nicknames #:mori-srv)
  (:export
   ;; API
   #:start
   #:enter-server-loop
   #:exit-server-loop
   #:unknown
   ;; server protocol
   #:on-init
   #:on-call
   #:on-cast
   #:on-message
   #:on-shutdown
   ;; Call
   #:*call-timeout*
   #:call
   #:reply
   #:multiple-value-reply
   #:defer-call-reply
   #:defcall
   #:callee-down
   #:call-timeout
   ;; Cast
   #:cast
   #:defcast))
(cl:in-package #:memento-mori.server)

;;;
;;; Server actor
;;;

(defgeneric on-init (driver)
  (:method ((driver t)) t))
(defgeneric on-call (driver request name args)
  (:method ((driver t) (request t) (name t) (args t))
    (error "No ON-CALL method defined for ~s with name ~s"
           driver name)))
(defgeneric on-cast (driver name args)
  (:method ((driver t) (name t) (args t))
    (error "No ON-CAST method defined for ~s with name ~s."
           driver name)))
(defgeneric on-message (driver message)
  (:method ((driver t) (message t))
    (error "No ON-MESSAGE method defined for ~s with message ~s."
           driver message)))
(defgeneric on-shutdown (driver reason)
  (:method ((driver t) (reason t)) t))

(defvar *in-server-loop-p* nil)

(defun exit-server-loop ()
  (unless *in-server-loop-p*
    (error "EXIT-SERVER-LOOP can only be called within the scope of a server loop."))
  (exit 'exit-server-loop))

(defun enter-server-loop (driver &aux reason (*in-server-loop-p* t))
  (assert (current-actor) () "ENTER-SERVER-LOOP must be called within the scope of an actor.")
  (handler-bind ((exit (lambda (e)
                         (setf reason e)
                         (when (eq 'exit-server-loop (exit-reason e))
                           (return-from enter-server-loop t))))
                 (error (lambda (e)
                          (setf reason (make-condition 'exit :reason e)))))
    (on-init driver)
    (unwind-protect
         (loop for msg = (receive)
            do (cond ((call-request-p msg)
                      (%handle-call-request driver msg))
                     ((cast-msg-p msg)
                      (%handle-cast-msg driver msg))
                     (t
                      (on-message driver msg))))
      (on-shutdown driver (or reason (make-condition 'exit :reason 'unknown))))))

(defun start (driver-function &key
                                linkp monitorp trap-exits-p
                                (name nil namep)
                                (initial-bindings *default-special-bindings*)
                                (debugp *debug-on-error-p*))
  (apply #'spawn
         (lambda () (enter-server-loop (funcall driver-function)))
         :linkp linkp
         :monitorp monitorp
         :trap-exits-p trap-exits-p
         :initial-bindings initial-bindings
         :debugp debugp
         (when namep (list :name name))))

;;;
;;; Call
;;;
(defstruct call-reply
  (request nil :read-only t)
  (values nil :read-only t))
(defstruct call-request
  (name nil :read-only t)
  (args nil :read-only t)
  (caller nil :read-only t)
  (monitor nil :read-only t))

(define-condition call-error (error) ())
(define-condition callee-down (call-error) ())
(define-condition call-timeout (call-error) ())

(defvar +defer-call-reply+ (gensym "DEFER-CALL-REPLY"))
(defun defer-call-reply ()
  (throw +defer-call-reply+ nil))

(defparameter *call-timeout* 5)

(defun call (actor name args &key (timeout *call-timeout*))
  (let ((request (make-call-request :monitor (monitor actor)
                                    :caller (current-actor)
                                    :name name
                                    :args args)))
    (send actor request)
    (receive-cond (reply)
      ((and (call-reply-p reply)
            (eq (call-reply-request reply) request))
       (demonitor (call-request-monitor request))
       (values-list (call-reply-values reply)))
      ((and (monitor-exit-p reply)
            (eq (call-request-monitor request)
                (monitor-exit-monitor reply)))
       (error 'callee-down))
      (after timeout (error 'call-timeout)))))

(defun reply (request &rest values)
  (send (call-request-caller request)
        (make-call-reply
         :request request
         :values values)))

(defmacro multiple-value-reply (request multiple-value-form)
  `(multiple-value-call 'reply ,request ,multiple-value-form))

(defun %handle-call-request (driver req)
  (catch +defer-call-reply+
    (send (call-request-caller req)
          (make-call-reply
           :request req
           :values (multiple-value-list
                    (on-call driver
                             req
                             (call-request-name req)
                             (call-request-args req)))))))

(defmacro defcall (name lambda-list
                   (server-var
                    server-class
                    &key
                    server-form
                    request
                    (define-function-p t)
                    (timeout nil timeoutp)) &body body)
  (let ((args-var (gensym "ARGS")))
    `(progn
       ,@(when define-function-p
               `((defun ,name ,(if server-form
                                   '(&rest args)
                                   `(,server-var &rest args))
                   ;; TODO - instead of &rest args, parse the lambda-list
                   ;;        so we get nice minibuffer hints for these
                   ;;        functions.
                   (call ,(or server-form server-var)
                         ',name args
                         ,@(when timeoutp `(:timeout ,timeout))))))
       (defmethod on-call ((,server-var ,server-class)
                           (,(or request (gensym "REQUEST")) t)
                           (,(gensym "NAME") (eql ',name))
                           ,args-var)
         (flet ((,name ,lambda-list ,@body))
           (apply #',name ,args-var)))
       ',name)))

;;;
;;; Cast
;;;
(defstruct cast-msg name args)
(defun cast (actor name args)
  (send actor (make-cast-msg :name name :args args)))

(defmacro defcast (name lambda-list
                   (server-var
                    server-class
                    &key
                    server-form
                    (define-function-p t))
                   &body body)
  (let ((args-var (gensym "ARGS")))
    `(progn
       ,@(when define-function-p
               `((defun ,name ,(if server-form
                                   '(&rest args)
                                   `(,server-var &rest args))
                   (cast ,(or server-form server-var) ',name args))))
       (defmethod on-cast ((,server-var ,server-class)
                           (,(gensym "NAME") (eql ',name))
                           ,args-var)
         (flet ((,name ,lambda-list ,@body))
           (apply #',name ,args-var)))
       ',name)))

(defun %handle-cast-msg (driver msg)
  (on-cast driver (cast-msg-name msg) (cast-msg-args msg)))

;;;
;;; Utils
;;;

;; TODO - actually use this
#+nil
(defmacro defhandler (callback-name call-name
                      name
                      (server-var
                       server-class
                       &key
                       (define-function-p t)
                       (timeout nil timeoutp))
                      lambda-list &body body)
  (let ((args-var (gensym "ARGS")))
    `(progn
       ,@(when define-function-p
               `((defun ,name (,server-var &rest args)
                   (,call-name ,server-var ',name args ,@(when timeoutp `(:timeout ,timeout))))))
       (defmethod ,callback-name ((,server-var ,server-class)
                                  (,(gensym "NAME") (eql ',name))
                                  ,args-var)
         (flet ((,name ,lambda-list ,@body))
           (apply #',name ,args-var)))))  )
