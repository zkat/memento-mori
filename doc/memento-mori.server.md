# Server

`memento-mori.server` (nickname `mori-srv`) implements a generic server for
client-server relations. `mori-srv` aims to abstract away standard
communication, startup, and shutdown patterns for actor loops. Its API is
based on Erlang's [`gen_server`](http://erldocs.com/R15B/stdlib/gen_server.html?i=0&search=gen_server#undefined) module.

All protocol functions are optional, but errors are signaled if a `call`,
`cast`, or regular message is received and a `driver` has no appropriate
methods to handle those sorts of messages. `on-init` and `on-shutdown` Have
default methods that do nothing.

## Server management

A server is started using the `start` function, which is similar to
`mori:spawn`, but accepts a function that is expected to return a `driver`
object. The `driver` object is then used to dispatch the various `mori-srv`
generic functions. An existing actor can also be converted to a server by
calling the `enter-server-loop` function.

A server may stop for various reasons, including the usual exits, kills,
and unhandled errors. Additionally, `exit-server-loop` may be used to exit
the server loop. This does not necessarily stop the actor, though, unless
it was started with `start`.

### Server management dictionary

#### *[generic function]* `on-init driver`

Called on `driver` when a server enters its server loop. Any initialization
of the `driver` object or other things should happen here.

#### *[generic function]* `on-shutdown driver reason`

Called on `driver` when the server is exiting for any reason. If the server
is exiting due to a standard exit or an unhandled error, `reason` will be
the exit reason. Otherwise, `reason` will have a reason of `'unknown`.

#### *[generic function]* `on-message driver message`

Called on `driver` when the server receives a non-`call`, non-`cast`
message. `message` is whatever Lisp object was sent.

##### *[function]* `start driver-function &key linkp monitorp trap-exits-p name initial-bindings debugp`
 
Creates a new actor and immediately enters a server-loop. `driver-function`
must be a function whose primary return value is a Lisp object that will be
used to dispatch the various 'callback' functions in `mori-srv`. This
function's keyword arguments are passed to `mori:spawn`, and the function
will return whatever `mori:spawn` returns (either an `actor` or `(values
actor monitor)`).

##### *[function]* `enter-server-loop driver`

Makes the current actor enter a server loop, using `driver` to dispatch
server callbacks. Once this function is called, the current actor will
behave like any other server, although it will not necessarily terminate
when the server loop is exited (since there may be more code to execute).

##### *[function]* `exit-server-loop`

Immediately exits from the current server's loop. If `enter-server-loop`
was used to convert an actor, `enter-server-loop` returns t and actor
execution continues. It is an error to call `exit-server-loop` outside of
the scope of a server (meaning, outside the body of a callback method).


## Cast - asynchronous messages

The casting API provides utilities for defining senders and handlers of
asynchronous messages. The main distinction between casts and regular sends
is that casting allows one to easily distinguish between unexpected and
system messages, and messages explicitly meant for a server's API. Aside
from this and the convenience of `defcast`, casting works identically to
standard sends.

### Cast dictionary

*[generic function]* `on-cast driver name args`

Called on `driver` whenever a cast message is received. `name` is the name
of the cast message (for example, a symbol), and `args` is a list
containing the additional arguments `cast` was called with.

*[function]* `cast actor name &rest args`

Sends a cast message to `actor` (an actor designator, not the driver
associated with that server). `name` should be some kind of identifier to
distinguish this message. `args` will be sent along with the message as
arbitrary additional arguments. Returns T as soon as the message has been
sent.

*[macro]* `defcast name lambda-list (server-var server-specializer &key server-form define-function-p) &body body`
 
Convenience macro for defining a pair of API function + cast
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

(defcast say-hello (&key (to "World"))
    (driver greeter)
  (format t "~&~A sez: 'Hello, ~A!'~%" driver to))

(say-hello (start #'make-greeter) :to "Detroit") => T
```

## Call - synchronous messages

Calls implement the synchronous messaging pattern. Roughly speaking, this
involves an actor sending a message to another actor and immediately doing
a blocking selective `receive` until it receives an appropriate response
from that actor.

Calls are made to remote actors by calling `call` on an actor
designator for a running server. The remote actor will then call the
appropriate `on-call` method for that message, and possibly reply  with
one or more values, which will be sent to the calling actor as the return
values of `call`.

When a server's `on-call` method is called, it may return one or more
values immediately, or it may defer the reply for a later time using
`defer-call-reply`, which breaks out of the `on-call` method
immediately. `reply` and `multiple-value-reply` may then be used in the
future with the `request` passed to `on-call` to finally respond to the
waiting actor. Deferring replies allows a server to free itself up for
processing other things while still blocking the calling actor until a
response is ready (if ever).

### Call dictionary

*[generic function]* `on-call driver request name args`

Called on `driver` whenever a call message is received. `name` is the name
of the cast message (for example, a symbol), and `args` is a list
containing the additional arguments `call` was called with.

If the `on-call` method returns, its return values will be sent to the
caller as the return values to `call`. If `on-call` returns non-locally, as
with an error signal or if `defer-call-reply` is called, no reply will be
made.

`request` is a unique reference to this particular request. It may be used
with `reply` and `multiple-value-reply` when a call is deferred to respond
to this specific call. Any process may respond to `request`, although
additional replies after the first one will end up as garbage messages in
the caller's mailbox. These messages will be of type `call-reply`, in the
interest of allowing cleanup.

*[function]* `call actor name args &key (timeout *call-timeout*)`

Sends a `call` request to `actor`, which should be a valid actor
designator. `name` is a lisp object used to identify the particular call
type, and `args` is a list to be passed to the `on-call` handler as the
arguments to the call. 

If and when `actor` replies to the call request, zero or more values will
be returned from the `call` call.

If `timeout` is non-`nil`, it must be a number denoting the number of
seconds to wait before `call` signals a `call-timeout` error. The default
is `*call-timeout*`, which defaults to `5` seconds.

A `callee-down` error is signaled If `actor` exits during the `call`.

*[function]* `defer-call-reply`

Immediately exits from the current `on-call` method. No values will be
returned to the waiting client, and the server will continue operating
normally. `reply` and `multiple-value-reply` may be used to respond to the
call at a later time.

*[function]* `reply request &rest values`

Sends `values` as the values returned from a `call` associated with
`request`. If `request` has already been responded to, the client may find
an object of type `call-reply` in its mailbox, which should be thrown
away. `reply` may be called from any actor, not necessarily the server who
originally received the request.

*[macro]* `multiple-value-reply request multiple-value-form`

Like `reply`, this responds to a call `request`. Unlike reply, this macro
sends multiple values returned from `multiple-value-form` in the
`call-reply`.

This macro is equivalent to `(multiple-value-call #'reply request multiple-value-form)`

*[variable]* `*call-timeout*`

Seconds to wait before signaling a `call-timeout` error. Defaults to `5`.

*[condition]* `call-error`

Parent condition type for call-related errors.

*[condition]* `callee-down`

Signaled when a server exits during a `call`.

*[condition]* `call-timeout`

Signaled when a `call` times out.

*[macro]* `defcall name lambda-list (server-var server-specializer &key server-form request define-function-p timeout) &body body`

Convenience macro for defining a pair of API function + call
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

(defcall say-hello (&key (to "World"))
    (driver greeter :request req)
  (values (list driver req to) to))

(say-hello (start #'make-greeter) :to "Detroit")
    =>
  (#S(GREETER) #<CALL-REQUEST #x302000F7FBDD> "Detroit")
  "Detroit"
```
