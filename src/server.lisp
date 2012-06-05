(cl:defpackage #:hipocrite.server
  (:use #:cl #:hipocrite)
  (:export
   ;; API
   #:start
   #:enter-server-loop
   #:exit-server-loop
   ;; server protocol
   #:on-init
   #:on-call
   #:on-cast
   #:on-direct-message
   #:on-terminate
   ;; Call
   #:call
   #:reply
   #:multiple-value-call
   #:defer-call-reply
   #:defcall
   ;; Cast
   #:cast
   #:defcast))
(cl:in-package #:hipocrite.server)

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
(defgeneric on-direct-message (driver message)
  (:method ((driver t) (message t))
    (error "No ON-CAST method defined for ~s with message ~s."
           driver message)))
(defgeneric on-terminate (driver reason)
  (:method ((driver t) (reason t)) t))

(defvar +server-loop-exit+ (gensym "SERVER-LOOP-EXIT"))
(defun exit-server-loop ()
  (throw +server-loop-exit+ nil))

(defun enter-server-loop (driver)
  (unwind-protect
       (catch +server-loop-exit+
         (on-init driver)
         (loop for msg = (receive)
            do (cond ((call-request-p msg)
                      (%handle-call-request driver msg))
                     ((cast-msg-p msg)
                      (%handle-cast-msg driver msg))
                     (t
                      (on-direct-message driver msg)))))
    (on-terminate driver (make-condition 'actor-exit
                                         :reason :normal))))

(defun start (driver &key linkp monitorp trap-exits-p
              (name nil namep)
              (debugp *debug-on-error-p*))
  (apply #'spawn
         (lambda () (enter-server-loop driver))
         :linkp linkp
         :monitorp monitorp
         :trap-exits-p trap-exits-p
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

(defun call (actor name args &key (timeout 5))
  (let ((request (make-call-request :monitor (monitor actor)
                                    :caller (current-actor)
                                    :name name
                                    :args args)))
    (send actor request)
    (receive-cond (reply :timeout timeout :on-timeout (error 'call-timeout))
      ((and (call-reply-p reply)
            (eq (call-reply-request reply) request))
       (demonitor (call-request-monitor request))
       (values-list (call-reply-values reply)))
      ((and (monitor-exit-p reply)
            (eq (call-request-monitor request)
                (monitor-exit-monitor reply)))
       (error 'callee-down)))))

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
                    request-var
                    (define-function-p t)
                    (timeout nil timeoutp)) &body body)
  (let ((args-var (gensym "ARGS")))
    `(progn
       ,@(when define-function-p
               `((defun ,name ,(if server-form
                                   '(&rest args)
                                   `(,server-var &rest args))
                   ;; TODO - instead of &rest args, parse the lambda-list so we get nice minibuffer
                   ;;        hints for these functions.
                   (call ,(or server-form server-var)
                         ',name args
                         ,@(when timeoutp `(:timeout ,timeout))))))
       (defmethod on-call ((,server-var ,server-class)
                           (,(or request-var (gensym "REQUEST")) t)
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
           (apply #',name ,args-var))))))

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
