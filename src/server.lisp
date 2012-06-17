(cl:defpackage #:memento-mori.server
  (:use #:cl #:alexandria #:memento-mori)
  (:nicknames #:mori-srv)
  (:export
   ;; server protocol
   #:on-init
   #:on-call
   #:on-cast
   #:on-message
   #:on-shutdown
   ;; API
   #:start
   #:enter-server-loop
   #:exit-server-loop
   #:unknown
   ;; Cast
   #:cast
   #:defcast
   ;; Call
   #:*call-timeout*
   #:call
   #:reply
   #:multiple-value-reply
   #:defer-call-reply
   #:call-request
   #:call-request-p
   #:call-reply
   #:call-reply-p
   #:defcall
   #:call-error
   #:callee-down
   #:call-timeout))
(cl:in-package #:memento-mori.server)

;;;
;;; Protocol
;;;
(defgeneric on-init (driver)
  (:method ((driver t)) t)
  (:documentation "Called on `driver` when a server enters its server
loop. Any initialization of the `driver` object or other things should
happen here."))
(defgeneric on-call (driver request name args)
  (:method ((driver t) (request t) (name t) (args t))
    (error "No ON-CALL method defined for ~s with name ~s"
           driver name))
  (:documentation "Called on `driver` whenever a call message is
received. `name` is the name of the cast message (for example, a symbol),
and `args` is a list containing the additional arguments `call` was called
with.

If the `on-call` method returns, its return values will be sent to the
caller as the return values to `call`. If `on-call` returns non-locally, as
with an error signal or if `defer-call-reply` is called, no reply will be
made.

`request` is a unique reference to this particular request. It may be used
with `reply` and `multiple-value-reply` when a call is deferred to respond
to this specific call. Any process may respond to `request`, although
additional replies after the first one will end up as garbage messages in
the caller's mailbox. These messages will be of type `call-reply`, in the
interest of allowing cleanup."))
(defgeneric on-cast (driver name args)
  (:method ((driver t) (name t) (args t))
    (error "No ON-CAST method defined for ~s with name ~s."
           driver name))
  (:documentation "Called on `driver` whenever a cast message is
received. `name` is the name of the cast message (for example, a symbol),
and `args` is a list containing the additional arguments `cast` was called
with."))
(defgeneric on-message (driver message)
  (:method ((driver t) (message t))
    (error "No ON-MESSAGE method defined for ~s with message ~s."
           driver message))
  (:documentation "Called on `driver` when the server receives a
non-`call`, non-`cast` message. `message` is whatever Lisp object was
sent."))
(defgeneric on-shutdown (driver reason)
  (:method ((driver t) (reason t)) t)
  (:documentation "Called on `driver` when the server is exiting for any
reason. If the server is exiting due to a standard exit or an unhandled
error, `reason` will be the exit reason. Otherwise, `reason` will have a
reason of `'unknown`."))

;;;
;;; Basic API
;;;
(defvar *in-server-loop-p* nil)

(defun start (driver-function &key
                                linkp monitorp trap-exits-p
                                (name nil namep)
                                (initial-bindings *default-special-bindings*)
                                (debugp *debug-on-error-p*))
  "Creates a new actor and immediately enters a
server-loop. `driver-function` must be a function whose primary return
value is a Lisp object that will be used to dispatch the various 'callback'
functions in `mori-srv`. This function's keyword arguments are passed to
`mori:spawn`, and the function will return whatever `mori:spawn`
returns (either an `actor` or `(values actor monitor)`)."
  (apply #'spawn
         (lambda () (enter-server-loop (funcall driver-function)))
         :linkp linkp
         :monitorp monitorp
         :trap-exits-p trap-exits-p
         :initial-bindings initial-bindings
         :debugp debugp
         (when namep (list :name name))))

(defun enter-server-loop (driver &aux reason (*in-server-loop-p* t))
  "Makes the current actor enter a server loop, using `driver` to dispatch
server callbacks. Once this function is called, the current actor will
behave like any other server, although it will not necessarily terminate
when the server loop is exited (since there may be more code to execute)."
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

(defun exit-server-loop ()
  "Immediately exits from the current server's loop. If `enter-server-loop`
was used to convert an actor, `enter-server-loop` returns t and actor
execution continues. It is an error to call `exit-server-loop` outside of
the scope of a server (meaning, outside the body of a callback method)."
  (unless *in-server-loop-p*
    (error "EXIT-SERVER-LOOP can only be called within the scope of a server loop."))
  (exit 'exit-server-loop))

;;;
;;; Cast
;;;
(defstruct cast-msg name args)

(defun cast (actor name &rest args)
  "Sends a cast message to `actor` (an actor designator, not the driver
associated with that server). `name` should be some kind of identifier to
distinguish this message. `args` will be sent along with the message as
arbitrary additional arguments. Returns T as soon as the message has been
sent."
  (send actor (make-cast-msg :name name :args args))
  t)

(defmacro defcast (name lambda-list
                   (server-var
                    server-specializer
                    &key
                    (server-form nil server-form-p)
                    (define-function-p t))
                   &body body)
"Convenience macro for defining a pair of API function + cast
handler. `defcast` defines an API function roughly expanding to `(defun
name (server &rest args) (apply #'cast server 'name args))`, as well as a
method for the `on-cast` generic function that `eql`-dispatches on `name`,
and executes `body` on the contents of the `cast` message.

  * `name` -- Used for both the name of the API function and the name to
    dispatch on when handling the `cast` message.
  * `lambda-list` -- Lambda list used for destructuring `args` in the cast
    handler, and for creating the API function's lambda list.
  * `server-var` -- Variable to bind the server to. For API functions, this
    variable will appear in its lambda list as the variable for the actor
    designator. For `on-cast` methods, this variable will be bound to the
    `driver` object.
  * `server-specializer` -- Specializer to dispatch the `driver` on (either
    the class name, or an `eql` specializer).
  * `server-form` -- If provided, the `server` argument will be omitted from
    the API function's lambda list. Instead, `server-form`'s return value
    will be sent the cast message.
  * `define-function-p` -- If `nil`, an API function will not be defined
    (but an `on-cast` method will). Defaults to true.
  * `body` -- Forms to use in the body of the `on-cast` method.

Example:

```lisp
 (defstruct greeter)

 (defcast say-hello (&key (to \"World\"))
     (driver greeter)
   (format t \"~&~A sez: 'Hello, ~A!'~%\" driver to))

 (say-hello (start #'make-greeter) :to \"Detroit\") => T
```"
  (let ((args-var (gensym "ARGS")))
    `(progn
       ,@(when define-function-p
               `((defun ,name ,(if server-form
                                   '(&rest args)
                                   `(,server-var &rest args))
                   (apply #'cast ,(if server-form-p server-form server-var) ',name args))))
       (defmethod on-cast ((,server-var ,server-specializer)
                           (,(gensym "NAME") (eql ',name))
                           ,args-var)
         (flet ((,name ,lambda-list ,@body))
           (apply #',name ,args-var)))
       ',name)))

(defun %handle-cast-msg (driver msg)
  (on-cast driver (cast-msg-name msg) (cast-msg-args msg)))

;;;
;;; Call
;;;
(defstruct call-reply
  (request nil :read-only t)
  (values nil :read-only t))
(defmethod print-object ((reply call-reply) stream)
  (print-unreadable-object (reply stream :type t :identity t)))

(defstruct call-request
  (name nil :read-only t)
  (args nil :read-only t)
  (caller nil :read-only t)
  (monitor nil :read-only t))
(defmethod print-object ((req call-request) stream)
  (print-unreadable-object (req stream :type t :identity t)))

(defparameter *call-timeout* 5
  "Seconds to wait before signaling a `call-timeout` error. Defaults to `5`.")

(defun call (actor name args &key (timeout *call-timeout*))
  "Sends a `call` request to `actor`, which should be a valid actor
designator. `name` is a lisp object used to identify the particular call
type, and `args` is a list to be passed to the `on-call` handler as the
arguments to the call.

If and when `actor` replies to the call request, zero or more values will
be returned from the `call` call.

If `timeout` is non-`nil`, it must be a number denoting the number of
seconds to wait before `call` signals a `call-timeout` error. The default
is `*call-timeout*`, which defaults to `5` seconds.

A `callee-down` error is signaled If `actor` exits during the `call`."
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

(defvar +defer-call-reply+ (gensym "DEFER-CALL-REPLY"))
(defun defer-call-reply ()
  "Immediately exits from the current `on-call` method. No values will be
returned to the waiting client, and the server will continue operating
normally. `reply` and `multiple-value-reply` may be used to respond to the
call at a later time."
  (ignore-errors (throw +defer-call-reply+ nil))
  (error "DEFER-CALL-REPLY must be called within ON-CALL."))

(defun reply (request &rest values)
  "Sends `values` as the values returned from a `call` associated with
`request`. If `request` has already been responded to, the client may find
an object of type `call-reply` in its mailbox, which should be thrown
away. `reply` may be called from any actor, not necessarily the server who
originally received the request."
  (send (call-request-caller request)
        (make-call-reply
         :request request
         :values values)))

(defmacro multiple-value-reply (request multiple-value-form)
  "Like `reply`, this responds to a call `request`. Unlike reply, this
macro sends multiple values returned from `multiple-value-form` in the
`call-reply`.

This macro is equivalent to `(multiple-value-call #'reply request
multiple-value-form)`"
  `(multiple-value-call 'reply ,request ,multiple-value-form))

(define-condition call-error (error) ())
(define-condition callee-down (call-error) ())
(define-condition call-timeout (call-error) ())

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
                    server-specializer
                    &key
                    (server-form nil server-form-p)
                    request
                    (define-function-p t)
                    (timeout nil timeoutp)) &body body)
"Convenience macro for defining a pair of API function + call
handler. `defcall` defines an API function roughly expanding to `(defun
name (server &rest args) (apply #'call server 'name args))`, as well as a
method for the `on-call` generic function that `eql`-dispatches on `name`,
and executes `body` on the contents of the `call` message.

  * `name` -- Used for both the name of the API function and the name to
    dispatch on when handling the `call` message.
  * `lambda-list` -- Lambda list used for destructuring `args` in the call
    handler, and for creating the API function's lambda list.
  * `server-var` -- Variable to bind the server to. For API functions, this
    variable will appear in its lambda list as the variable for the actor
    designator. For `on-call` methods, this variable will be bound to the
    `driver` object.
  * `server-specializer` -- Specializer to dispatch the `driver` on (either
    the class name, or an `eql` specializer).
  * `server-form` -- If provided, the `server` argument will be omitted from
    the API function's lambda list. Instead, `server-form`'s return value
    will be sent the call message.
  * `request` -- Variable to bind the request object to in the call
    handler.
  * `define-function-p` -- If `nil`, an API function will not be defined
    (but an `on-call` method will). Defaults to true.
  * `timeout` -- If non-`nil`, must be the number of seconds to wait before
    the `call` times out (and signals `'call-timeout`).
  * `body` -- Forms to use in the body of the `on-call` method.

Example:

```lisp
 (defstruct greeter)

 (defcall say-hello (&key (to \"World\"))
     (driver greeter :request req)
   (values (list driver req to) to))

 (say-hello (start #'make-greeter) :to \"Detroit\")
     =>
   (#S(GREETER) #<CALL-REQUEST #x302000F7FBDD> \"Detroit\")
   \"Detroit\"
```"
  (let ((args-var (gensym "ARGS")))
    `(progn
       ,@(when define-function-p
               `((defun ,name ,(if server-form
                                   '(&rest args)
                                   `(,server-var &rest args))
                   ;; TODO - instead of &rest args, parse the lambda-list
                   ;;        so we get nice minibuffer hints for these
                   ;;        functions.
                   (call ,(if server-form-p server-form server-var)
                         ',name args
                         ,@(when timeoutp `(:timeout ,timeout))))))
       (defmethod on-call ((,server-var ,server-specializer)
                           (,(or request (gensym "REQUEST")) t)
                           (,(gensym "NAME") (eql ',name))
                           ,args-var)
         (flet ((,name ,lambda-list ,@body))
           (apply #',name ,args-var)))
       ',name)))
