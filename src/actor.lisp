(defpackage #:memento-mori
  (:use #:cl #:alexandria #:memento-mori.utils)
  (:import-from #:memento-mori.mailbox #:receive-timeout)
  (:import-from #:bordeaux-threads #:*default-special-bindings*)
  (:nicknames #:mori)
  (:export
   ;; Debugging
   #:*debug-on-error-p*
   #:enable-crash-logging
   #:disable-crash-logging
   #:crash-logging-enabled-p
   ;; Actors
   #:current-actor
   #:*default-special-bindings*
   #:actor
   #:all-actors
   #:actor-alive-p
   #:spawn
   #:call-with-actor-context
   #:with-actor-context
   ;; Messaging
   #:send
   #:receive
   #:receive-cond
   #:receive-timeout
   #:selective-receive
   #:flush-messages
   ;; Exits
   #:exit
   #:exit-reason
   #:remote-exit
   #:remote-exit-p
   #:remote-exit-from
   #:remote-exit-reason
   #:finished
   #:shutdown
   #:kill
   #:killed
   #:trap-exits-p
   #:enable-trap-exits
   #:disable-trap-exits
   #:break-actor
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
   ;; Monitoring
   #:monitor
   #:demonitor
   #:monitorp
   #:monitor-monitored-actor
   #:monitor-exit
   #:monitor-exit-p
   #:monitor-exit-monitor
   #:monitor-exit-from
   #:monitor-exit-reason))
(in-package #:memento-mori)

;;;
;;; Actors
;;;
(defvar *debug-on-error-p* nil)
(defvar *current-actor* nil)

(defstruct actor
  (mailbox (memento-mori.mailbox:make-mailbox) :read-only t)
  (monitor-lock (bt:make-lock) :read-only t)
  monitors
  name
  named-p
  links
  (exit-lock (bt:make-lock) :read-only t)
  trap-exits-setting
  thread
  function)

(defmethod print-object ((actor actor) stream)
  (print-unreadable-object (actor stream :type t :identity t)
    (maybe-format-actor-name actor stream)
    (format stream "[~a msgs]" (memento-mori.mailbox:mailbox-count
                                (actor-mailbox actor)))))

(defun current-actor ()
  "Returns the current actor object, or NIL if called outside of an actor thread."
  *current-actor*)

(defun actor-alive-p (actor)
  (bt:thread-alive-p (actor-thread (ensure-actor actor))))

(defvar *all-actors* ())
(defvar *all-actors-lock* (bt:make-lock))
(defmacro with-all-actors-lock (&body body)
  `(bt:with-recursive-lock-held (*all-actors-lock*)
     ,@body))

(defun all-actors ()
  "Returns a list of all living actors."
  (with-all-actors-lock (copy-list *all-actors*)))

(defun %add-actor (actor)
  (with-all-actors-lock (push actor *all-actors*)))

(defun %delete-actor (actor)
  (with-all-actors-lock (deletef *all-actors* actor :test #'eq)))

(defun spawn (func &key
                     linkp monitorp trap-exits-p
                     (name nil namep) (debugp *debug-on-error-p*)
                     (initial-bindings *default-special-bindings*))
  "Executes `function` within the scope of a new actor thread. The various
keyword arguments to `spawn` allow certain operations to take effect
immediately/atomically on actor creation, instead of risking an actor dying
before the related API functions can be called. Note that linking and
monitoring will fail if the thread calling `spawn` is not an actor thread.

  * `function` -- a valid function designator for a zero-argument
    function.
  * `linkp` -- If true, immediately links the current actor with the newly
    created actor.
  * `monitorp` -- If true, the current actor will immediately begin
    monitoring the new actor, and the monitor reference will be the second
    return value for `spawn`.
  * `trap-exits-p` -- If true, the new actor will immediately begin
    trapping remote exit signals.
  * `name` -- A symbol to use to globally register the new actor.
  * `initial-bindings` -- An alist of `(symbol . value)` to use as the
     initial dynamic variable bindings for the new actor.
  * `debugp` -- If true, `invoke-debugger` will be called on unhandled
    errors. The `debugp` option will be inherited by any actors created by
    the new actor, and any actors those actors create (and `:debugp nil`
    can be passed to stop this cascade of debugger love)."
  (when namep
    (check-type name symbol "a valid actor name"))
  (let* ((actor (make-actor :function func :trap-exits-setting trap-exits-p))
         (monitor (when monitorp (%monitor actor (current-actor) nil))))
    (when namep (register name actor))
    (%add-actor actor)
    (setf (actor-thread actor)
          (bt:make-thread
           (make-actor-function actor func linkp namep name debugp)
           :name (format nil "Mori actor thread for ~s" actor)
           :initial-bindings
           (list*
            (cons '*current-actor* actor)
            (cons '*debug-on-error-p* *debug-on-error-p*)
            initial-bindings)))
    (if monitor
        (values actor monitor)
        actor)))

(defun call-with-actor-context (function
                                &key
                                  trap-exits-p
                                  (name nil namep)
                                  (debugp *debug-on-error-p*))
  "Calls `function`, a no-argument function, in the scope of an actor, and
returns once `function` completes. The 'fake' actor will behave as if it
terminated on return. Returns the actor's exit reason."
  (let ((*current-actor* (make-actor :function function
                                     :trap-exits-setting trap-exits-p
                                     :thread (bt:current-thread))))
    (when namep (register name *current-actor*))
    (%add-actor *current-actor*)
    (funcall (make-actor-function *current-actor* function nil namep name debugp))))

(defmacro with-actor-context ((&key
                               trap-exits-p (name nil namep) (debugp *debug-on-error-p*))
                                 &body body)
  "Executes `body` in an implicit `progn` within the scope of an actor, and
returns once execution completes. The 'fake' actor will be have as if it
terminated on return. Returns the actor's exit reason."
  `(call-with-actor-context (lambda () ,@body)
                            :trap-exits-p ,trap-exits-p
                            :debugp ,debugp
                            ,@(when namep `(:name ,name))))

(let ((log-crashes-p t)
      (log-settings-lock (bt:make-lock)))
  (defun enable-crash-logging ()
    "For debugging. The logger will print crash notifications to `*error-output*`
when an actor exits for any reason other than `'shutdown` or `'finished`."
    (bt:with-recursive-lock-held (log-settings-lock)
      (setf log-crashes-p t)))
  (defun disable-crash-logging ()
    "Stops logger printouts to `*error-output*` when actors exit abnormally."
    (bt:with-recursive-lock-held (log-settings-lock)
      (setf log-crashes-p nil)))
  (defun crash-logging-enabled-p ()
    "Returns true if abnormal exit logging to `*error-output*` is currently
enabled."
    (bt:with-recursive-lock-held (log-settings-lock)
      log-crashes-p)))

(defvar *debugger-lock* (bt:make-lock))
(defvar *unhandled-exit* (gensym "UNHANDLED-EXIT"))
(defun make-actor-function (actor func linkp namep name debugp
                            &aux (parent (current-actor)))
  (lambda ()
    (without-interrupts
      (let (exit)
        (unwind-protect
             (setf exit
                   (block run-actor-function
                     (catch *unhandled-exit*
                       (handler-bind ((exit (lambda (exit)
                                              (return-from run-actor-function exit)))
                                      (%killed (lambda (killed)
                                                 (declare (ignore killed))
                                                 (return-from run-actor-function
                                                   (make-condition 'exit
                                                                   :reason 'killed))))
                                      (error (lambda (e)
                                               (when debugp
                                                 (bt:with-recursive-lock-held (*debugger-lock*)
                                                   (invoke-debugger e)))
                                               (return-from run-actor-function
                                                 (make-condition 'exit :reason e)))))
                         (restart-case
                             (#+sbcl sb-sys:allow-with-interrupts
                                     #-sbcl progn
                                     (when linkp (link parent))
                                     (with-interrupts (funcall func))
                                     (make-condition 'exit :reason 'finished))
                           (abort ()
                             :report "Kill the current actor."
                             (kill (current-actor))))))))
          (when namep (unregister name nil))
          (notify-links actor exit)
          (notify-monitors actor exit)
          (when (crash-logging-enabled-p)
            (log-crash actor exit))
          (%delete-actor actor))))))

(defun log-crash (actor exit &aux (reason (exit-reason exit)))
  (cond ((eq 'killed reason)
         (mori-log:warn "Actor ~a killed." actor))
        ((eq 'error reason)
         (mori-log:error "Actor ~a shutting down due to error: ~a"
                         actor (exit-reason exit)))
        ((or (eq 'shutdown reason)
             (eq 'finished reason))
         nil)
        (t
         (mori-log:warn "Actor ~a exited abnormally: ~a"
                        actor (exit-reason exit)))))

;;;
;;; Messaging
;;;
(defun send (actor message)
  "Puts `message` in `actor`'s mailbox and wakes it if it's waiting for new
messages. `message` is not copied when sent, so mutating it must be done
with great care, if at all."
  (memento-mori.mailbox:send (actor-mailbox (ensure-actor actor)) message))

(defun receive (&key timeout on-timeout)
  "Returns the most recent message in `(current-actor)`'s mailbox. If no
message is present, `receive` will block until a message is
received. `timeout` and `on-timeout` can be used to limit the amount of
time spent waiting for messages, instead of waiting infinitely.

  * `timeout` -- Either `nil`, representing 'infinity', or a number
  representing the number of seconds to wait until the `receive` call times
  out. When a `receive` times out, either a condition of type
  `receive-timeout` is signaled, or `on-timeout` is called and its values
  returned. Defaults to `nil`.
  * `on-timeout` -- Either `nil` or a zero-argument function to call when
  `timeout` expires. Does nothing if `timeout` is `nil`."
  (memento-mori.mailbox:receive (actor-mailbox (current-actor))
                                :timeout timeout
                                :on-timeout on-timeout))

(defmacro receive-cond ((value-var)
                        &body clauses)
  "Tries to match one of the tests in `clauses` with a message in
`(current-actor)`'s mailbox. The value from the mailbox is bound to
`value-var` within the scope of `clauses`. The first matching test will
execute its body forms as the return value of `receive-cond`.

If the test form is the symbol `after`, the second element must evaluate to
a number to be used as `receive-cond`'s timeout, and the rest of the forms
will be executed as an implicit `progn` to yield `receive-cond`'s return
value(s).

`receive-cond` is different from doing `(let ((val (receive))) (cond ...))`
in that it is able to 'pick' messages out of anywhere in the actor's
message queue, while still leaving all other messages in-place, in the
order they arrived.

Example:

```lisp
 (receive-cond (msg)
  ((eq msg 'hello-world)
   (print \"Oh, hello!\"))
  (after 10
   (print \"I'm not interested in waiting this long!\")))
```"
  `(memento-mori.mailbox:receive-cond (,value-var (actor-mailbox (current-actor)))
     ,@clauses))

(defun flush-messages ()
  "Flushes all messages from `(current-actor)`'s message queue, throwing them
away into nothingness."
  (receive :timeout 0 :on-timeout (lambda () (return-from flush-messages t)))
  (flush-messages))

(defun selective-receive (test &key timeout on-timeout)
"This function can be used to implement custom selective receive
operators similar to `receive-cond`. For example, it can be used to
implement a macro that picks a message from the mailbox using a given
pattern-matching library.

`test` must be a function that accepts a message as its only argument. If
it returns `nil`, the message will be skipped and `test` will be called on
the next message, until a matching message is found (or `selective-receive`
will block until a matching message is received). If the message is
accepted, `test` must return a one-argument function that will then be
called on the chosen message and whose values will be returned from
`selective-receive`.

`timeout` and `on-timeout` work as described in `receive`.

For example, on could write a `receive-match` macro that uses the `optima`
pattern-matching library:

```lisp
 (defmacro receive-match (&body clauses &aux timeout on-timeout)
  (alexandria:with-unique-names (obj)
    `(mori:selective-receive
      (lambda (,obj)
        (optima:match ,obj
          ,@(loop for (pattern . body) in clauses
               if (and (symbolp pattern)
                       (string-equal 'after pattern))
               do (when on-timeout
                    (warn \"Multiple AFTER clauses in RECEIVE-MATCH\"))
                 (setf timeout (car body)
                       on-timeout `(lambda () ,@(cdr body)))
               else
               collect `(,pattern (lambda (,obj)
                                    (declare (ignore ,obj))
                                    ;; Doing it this way allows
                                    ;; declarations related to the
                                    ;; THEN-form.
                                    (lambda ()
                                      ,@body))))))
      :timeout ,timeout
      :on-timeout ,on-timeout)))
```

Which can then be used like:

```lisp
 (receive-match
  (1 (print \"Got a 1\"))
  ((list x y z) (values x y z))
  (after 5 (print \"Whatever\")))
```"
  (memento-mori.mailbox:selective-receive
   (actor-mailbox (current-actor))
   test
   :timeout timeout
   :on-timeout on-timeout))

;;;
;;; Exits
;;;

(define-condition exit (condition)
  ((reason :initarg :reason :reader exit-reason)))

(define-condition %killed () ())

(defmethod print-object ((exit exit) stream)
  (print-unreadable-object (exit stream :type t)
    (format stream "~s" (exit-reason exit))))

(defstruct remote-exit
  (from nil :read-only t)
  (reason nil :read-only t))

(defmethod print-object ((remote-exit remote-exit) stream)
  (print-unreadable-object (remote-exit stream :type t)
    (format stream "~S"
            (remote-exit-reason remote-exit))))

(defun send-exit-message (actor exit)
  (send actor (make-remote-exit
               :from (current-actor)
               :reason (exit-reason exit))))

(defun signal-exit (actor exit &aux (actor (ensure-actor actor)))
  (cond ((eq actor (current-actor))
         (signal exit))
        ((and (%trap-exits-p actor)
              (not (typep exit '%killed)))
         (send-exit-message actor exit))
        (t
         (bt:interrupt-thread (actor-thread actor)
                              (lambda ()
                                (without-interrupts
                                  (throw *unhandled-exit* exit)))))))

(defun exit (reason &optional (actor (current-actor)))
  "Signals an exit to `actor`. If `actor` is `(current-actor)`, a Lisp
condition of type `exit` will be signaled, which can be handled like any
other regular condition. `actor` must be an actor designator.

If `actor` is a remote actor, an exit signal will be sent to it. If
`trap-exits-p remote-actor` is true, `remote-actor` will receive a
`remote-exit` object in its mailbox. Otherwise, it will be terminated with
`reason`."
  (signal-exit (ensure-actor actor)
               (if (eq 'kill reason)
                   (make-condition '%killed)
                   (make-condition 'exit :reason reason))))

(defun kill (&optional (actor (current-actor)))
  "Kills `actor`. This exit cannot be handled or trapped. It will terminate
`actor` with extreme prejudice. `actor` must be an actor designator. Actors
who are linked to `actor` or monitoring it will receive `'killed` as the
exit reason."
  (exit 'kill (ensure-actor actor)))

(defun %trap-exits-p (actor)
  (bt:with-recursive-lock-held ((actor-exit-lock (ensure-actor actor)))
    (actor-trap-exits-setting actor)))

(defun trap-exits-p (&aux (actor (current-actor)))
  "Returns true if the current actor is trapping exits."
  (%trap-exits-p actor))

(defun enable-trap-exits (&aux (actor (current-actor)))
  "Enables exit trapping for the current actor."
  (bt:with-recursive-lock-held ((actor-exit-lock actor))
    (setf (actor-trap-exits-setting actor) t)))

(defun disable-trap-exits (&aux (actor (current-actor)))
  "Disables exit trapping for the current actor."
  (bt:with-recursive-lock-held ((actor-exit-lock actor))
    (setf (actor-trap-exits-setting actor) nil)))

(defun break-actor (actor &optional string &rest args)
  "Interrupts `actor` with `cl:break`. Intended purely for debugging
purposes. `actor` must be an actor designator."
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
  "Returns a list of all registered actor names."
  (bt:with-recursive-lock-held (*registration-lock*)
    (hash-table-keys *registered-actors*)))

(defun find-actor (name &optional (errorp t))
  "Returns an `actor` named by `name`. If `errorp` is true, signals a
condition of type `no-such-actor` when an actor is not found under that
name. If `errorp` is false, returns nil when no actor is found."
  (check-type name symbol "a valid actor name")
  (bt:with-recursive-lock-held (*registration-lock*)
    (multiple-value-bind (actor foundp)
        (gethash name *registered-actors*)
      (cond (foundp actor)
            (errorp (error 'no-such-actor :name name))
            (t nil)))))

(defun register (name actor &optional (errorp t))
  "Registers `actor` under `name`. If `errorp` is true, signals a condition of
type `actor-already-exists` if there is already an actor registered under
that name. Otherwise, if `errorp` is false, the existing registration is
replaced."
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
            (exit 'shutdown old-actor))
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
  "Removes the actor registration associated with `name`. If `errorp` is true,
signals a condition of type `no-such-actor` if there is no actor registered
under that name. If false, returns nil."
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

(defun link (actor &aux (self (current-actor)))
  "Creates a link between `actor` and `(current-actor)`. If a link already
exists, nothing happens. Links do not stack."
  (let ((actor (ensure-actor actor)))
    (bt:with-recursive-lock-held (*link-lock*)
      (assert (actor-alive-p actor) ()
              "Cannot link to a dead actor.")
      (pushnew actor (actor-links self))
      (pushnew self (actor-links actor)))
    (values)))

(defun unlink (actor &aux (self (current-actor)))
  "Removes a link between `actor` and `(current-actor)`, if any."
  (let ((actor (ensure-actor actor)))
    (bt:with-recursive-lock-held (*link-lock*)
      (assert (actor-alive-p actor)
              ()
              "Cannot unlink from a dead actor.")
      (removef (actor-links actor) self)
      (removef (actor-links self) actor))
    (values)))

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
(defstruct (monitor (:predicate monitorp))
  (observer nil :read-only t)
  (monitored-actor nil :read-only t))
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
  "Makes `(current-actor)` monitor `actor`. If `actor` exits,
`(current-actor)` will receive a `monitor-exit` message. Any number of
independent monitors can be established between the same actor pair, and
they will all exist as individual monitors, and send an exit signal once
for each existing monitor.

Returns a `monitor` object, used for demonitoring and connecting
`monitor-exit` messages with monitor instances."
  (%monitor actor self t))

(defun demonitor (ref)
  "Disables `monitor`, preventing it from sending an associated `monitor-exit`
when the associated actor shuts down. Other monitors for the same actor
will continue to exist."
  (let ((actor (monitor-monitored-actor ref)))
    (bt:with-recursive-lock-held ((actor-monitor-lock actor))
      (removef (actor-monitors actor) ref))
    (values)))

(defstruct monitor-exit
  (monitor nil :read-only t)
  (from nil :read-only t)
  (reason nil :read-only t))
(defmethod print-object ((monitor-exit monitor-exit) stream)
  (print-unreadable-object (monitor-exit stream :type t)
    (format stream "~S"
            (monitor-exit-reason monitor-exit))))

(defun notify-monitors (actor exit)
  (bt:with-recursive-lock-held ((actor-monitor-lock actor))
    (loop for monitor in (actor-monitors actor)
       do (send (monitor-observer monitor)
                (make-monitor-exit :monitor monitor
                                   :from actor
                                   :reason (exit-reason exit))))
    (setf (actor-monitors actor) nil)))
