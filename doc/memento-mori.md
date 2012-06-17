# Memento Mori

The 'core' `memento-mori` API is available through the `memento-mori`
(nickname `mori`) package. This package covers the basics of creating and
managing actors.

## Actors

Actors are objects of type `actor` representing basic units of
computation... or whatever. Basically, they're like threads on steroids and
they can send messages to each other. Actors are created by calling the
`spawn` function, which executes a given function in the scope of an actor.

Actors in `mori` execute in regular Lisp threads (as wrapped by the
`bordeaux-threads` library), which means they're relatively heavyweight
compared to Erlang processes. It also means that memory isolation is not
enforced in the same way, so a bit more prudence is required while using
`mori`. Not like Erlang is truly shared-nothing, anyway. :)

*[function]* `spawn function &key linkp monitorp trap-exits-p name debugp`

Executes `function` within the scope of a new actor thread. The various
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
  * `debugp` -- If true, `invoke-debugger` will be called on unhandled
    errors. The `debugp` option will be inherited by any actors created by
    the new actor, and any actors those actors create (and `:debugp nil`
    can be passed to stop this cascade of debugger love).

*[function]* `current-actor`

Returns the current actor object, or NIL if called outside of an actor
thread.

*[function]* `actor-alive-p actor`

Returns a true value if `actor`'s thread is currently alive.

*[function]* `all-actors`

Returns a list of all living actors.

*[function]* `enable-crash-logging`

For debugging. The logger will print crash notifications to `*error-output*`
when an actor exits for any reason other than `'shutdown` or `'finished`.

*[function]* `disable-crash-logging`

Stops logger printouts to `*error-output*` when actors exit abnormally.

*[function]* `crash-logging-enabled-p`

Returns true if abnormal exit logging to `*error-output*` is currently
enabled.

## Messaging

Messages can be sent between actors by calling `send` and `receive` (and
`receive-cond`). Sending a message to an actor puts that message (any Lisp
object) in its mailbox. Messages are processed by `receive` and
`receive-cond` in the order they arrive, although `receive-cond` can 'pick'
messages from anywhere in the mailbox. It is important to note that `send`
does *not* copy data, so you're on your own if you decide to mutate objects
that have been passed around between actors.

*[function]* `send actor message`

Puts `message` in `actor`'s mailbox and wakes it if it's waiting for new
messages. `message` is not copied when sent, so mutating it must be done
with great care, if at all. `actor` must be an actor designator.

*[function]* `receive &key timeout on-timeout`

Returns the most recent message in `(current-actor)`'s mailbox. If no
message is present, `receive` will block until a message is
received. `timeout` and `on-timeout` can be used to limit the amount of
time spent waiting for messages, instead of waiting infinitely.

  * `timeout` -- Either `nil`, representing 'infinity', or a number
  representing the number of seconds to wait until the `receive` call times
  out. When a `receive` times out, either a condition of type
  `receive-timeout` is signaled, or `on-timeout` is called and its values
  returned. Defaults to `nil`.
  * `on-timeout` -- Either `nil` or a zero-argument function to call when
  `timeout` expires. Does nothing if `timeout` is `nil`.

*[macro]* `receive-cond (value-var) &body clauses`

Tries to match one of the tests in `clauses` with a message in
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
   (print "Oh, hello!"))
  (after 10
   (print "I'm not interested in waiting this long!")))
```

*[function]* `flush-messages`

Flushes all messages from `(current-actor)`'s message queue, throwing them
away into nothingness.

*[function]* `selective-receive test &key timeout on-timeout`

This function can be used to implement custom selective receive
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
                    (warn "Multiple AFTER clauses in RECEIVE-MATCH"))
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
  (1 (print "Got a 1"))
  ((list x y z) (values x y z))
  (after 5 (print "Whatever")))
```

## Exits

In `memento-mori`, actors terminate due to `exit`s. There are two main
types of exits: Standard `exit`s are actor terminations associated with
some specific reason, and 'kills', which are meant to be used to make
pretty sure something simply stops running. Most actor terminations will
involve standard exits, and the exit objects will have some sort of reason
attached. The main differences between standard exits and kills are that:
a) exits have a reason associated with them, b) kills cannot be handled, so
they always terminate the actor.

Standard exits can be either normal or abnormal. A normal exit is an exit
whose reason is either `finished`, meaning the actor successfully completed
executing its actor function, or `shutdown`, which means a request was sent
to this actor for it to shut down in a clean, orderly manner. An abnormal
exit is any exit with a reason other than `finished` or `shutdown`. Local
exits are Lisp conditions of type `exit`, so they can be handled using
`handler-bind`, `handler-case`, etc.

In addition to local exits and kills, actors may 'signal' exits to other
actors. These remote exits cannot be handled like conditions. In order to
prevent a remote exit from stopping an actor, the actor must trap
exits. Trapping can be enabled through `enable-trap-exits` (or the
`:trap-exits-p` option to `spawn`). If `trap-exits-p` is true, the actor
will receive an exit message of type `remote-exit` instead of shutting
down.

### Warning about interrupts and unwind-protect

`memento-mori`'s signaling mechanisms are implemented using thread
interrupts. This means that an actor may be interrupted in the middle of
something important, and the system may end up with inconsistent
state/database/etc. In some Lisp implementations (for example, in SBCL),
even `unwind-protect` may be insufficient to properly ensure that forms
will execute regardless of interrupts.

In order to make absolutely sure certain forms are not interrupted,
interrupts must be disabled (and `mori` currently does not export its own
portable interrupt-control API). This opens up its own can of worms,
though, since an infinite loop inside a form protected from interrupts will
render the thread completely unkillable until the entire Lisp image is
restarted. This is a particular risk in Clozure CL, since its
implementation of `unwind-protect` wraps the cleanup forms in
`without-interrupts`.

*[condition]* `exit`

Signaled by local calls to the `exit` function. If unhandled, the actor
will terminate with reason `exit-reason exit`.

*[function]* `exit reason &optional (actor (current-actor))`

Signals an exit to `actor`. If `actor` is `(current-actor)`, a Lisp
condition of type `exit` will be signaled, which can be handled like any
other regular condition. `actor` must be an actor designator.

If `actor` is a remote actor, an exit signal will be sent to it. If
`trap-exits-p remote-actor` is true, `remote-actor` will receive a
`remote-exit` object in its mailbox. Otherwise, it will be terminated with
`reason`.

*[function]* `kill &optional (actor (current-actor))`

Kills `actor`. This exit cannot be handled or trapped. It will terminate
`actor` with extreme prejudice. `actor` must be an actor designator. Actors
who are linked to `actor` or monitoring it will receive `'killed` as the
exit reason.

*[function]* `remote-exit-p exit`

Returns true if `exit` is an object of type `remote-exit`.

*[function]* `remote-exit-from exit`

Returns the actor who sent `exit`.

*[function]* `remote-exit-reason exit`

Returns `exit`'s reason.

*[function]* `trap-exits-p`

Returns true if the current actor is trapping exits.

*[function]* `enable-trap-exits`

Enables exit trapping for the current actor.

*[function]* `disable-trap-exits`

Disables exit trapping for the current actor.

*[function]* `break-actor actor &optional string &rest args`

Interrupts `actor` with `cl:break`. Intended purely for debugging
purposes. `actor` must be an actor designator.

## Registration

Actors can also be registered, which means a symbol can be assigned to them
as their global name. Registered actors can be looked up globally by that
single name. The actor designators that most `mori` functions accept are
either `actor` objects or registered actor names. Actors can either be
registered using the `register` function, or immediately registered using
the `:name` argument to `spawn`.

*[function]* `list-registered-named`

Returns a list of all registered actor names.

*[function]* `find-actor name &optional (errorp t)`

Returns an `actor` named by `name`. If `errorp` is true, signals a
condition of type `no-such-actor` when an actor is not found under that
name. If `errorp` is false, returns nil when no actor is found.

*[function]* `register name actor &optional (errorp t)`

Registers `actor` under `name`. If `errorp` is true, signals a condition of
type `actor-already-exists` if there is already an actor registered under
that name. Otherwise, if `errorp` is false, the existing registration is
replaced.

*[function]* `unregister name &optional (errorp t)`

Removes the actor registration associated with `name`. If `errorp` is true,
signals a condition of type `no-such-actor` if there is no actor registered
under that name. If false, returns nil.

## Linking and Monitoring

Actors may be mutually linked, which sends linked actors an exit when
their associate dies, or actors can monitor other actors, which means they
simply receive a message when the actor being monitored exits. A key
difference between links and monitors is that while only one link can exist
between two actors, a single actor can monitor another actor multiple
times.

*[function]* `link actor`

Creates a link between `actor` and `(current-actor)`. If a link already
exists, nothing happens. Links do not stack.

*[function]* `unlink actor`

Removes a link between `actor` and `(current-actor)`, if any.

*[function]* `monitor actor`

Makes `(current-actor)` monitor `actor`. If `actor` exits,
`(current-actor)` will receive a `monitor-exit` message. Any number of
independent monitors can be established between the same actor pair, and
they will all exist as individual monitors, and send an exit signal once
for each existing monitor.

Returns a `monitor` object, used for demonitoring and connecting
`monitor-exit` messages with monitor instances.

*[function]* `demonitor monitor`

Disables `monitor`, preventing it from sending an associated `monitor-exit`
when the associated actor shuts down. Other monitors for the same actor
will continue to exist.

*[function]* `monitorp monitor`

Returns true if `monitor` is of type `monitor`. Monitors act as references
for monitor links.

*[function]* `monitor-monitored-actor monitor`

The actor `monitor` is 'watching'.

*[function]* `monitor-exit-p monitor-exit`

Returns true if `monitor-exit` is of type `monitor-exit`.

*[function]* `monitor-exit-monitor monitor-exit`

Returns the `monitor` object associated with this `monitor-exit`. Useful to
check that a received `monitor-exit` is associated with a particular
monitor action.

*[function]* `monitor-exit-from monitor-exit`

Returns the actor whose shutdown triggered the `monitor-exit` message.

*[function]* `monitor-exit-reason monitor-exit`

The monitored actor's exit reason.
