# Quickstart

`memento-mori` is [hosted at Github](http://github.com/zkat/memento-mori).

```lisp
CL-USER> (ql:quickload 'memento-mori)
To load "memento-mori":
  Load 1 ASDF system:
    memento-mori
; Loading "memento-mori"

(memento-mori)
CL-USER> (Let ((actor (mori:spawn (lambda () (print (mori:receive))))))
           (mori:send actor "Hello, world!"))

"Hello, world!" T
CL-USER> (defstruct example-server)
EXAMPLE-SERVER
CL-USER> (mori-srv:defcall this-is-synchronous (this-is-an-argument)
             (server example-server)
           (format t "~&Server actor: ~s. Argument: ~s~%"
                   (mori:current-actor) this-is-an-argument)
           'a-return-value)

THIS-IS-SYNCHRONOUS
CL-USER> (let ((server (mori-srv:start #'make-example-server)))
           (mori:spawn (lambda ()
                        (format t "~&Caller: ~s.~%" (mori:current-actor))
                        (print
                         (this-is-synchronous server 'an-argument))
                        (mori:kill server))))
Caller: #<ACTOR [0 msgs] #x302000EA248D>.
Server actor: #<ACTOR [0 msgs] #x302000EA369D>. Argument: AN-ARGUMENT

A-RETURN-VALUE
#<ACTOR [0 msgs] #x302000EA248D>
NIL
```

# Introduction

`memento-mori` is a library for writing robust, agent-based systems in Common
Lisp. It draws inspiration from [Erlang/OTP](http://www.erlang.org),
particularly its generic behaviors. `memento-mori` includes utilities to
easily build call-and-response servers and supervision trees, along with
the lower-level concurrency primitives like actor spawning, messaging, and
inter-actor linking and monitoring. The concurrency is really just a
bonus. :)

The library is still under heavy development, and the API is not yet
documented. If you're curious enough, you can check out the test/ directory
for some interesting examples. Feel free to contact me with questions or
ideas.

# Packages

* `memento-mori` (aka `mori`) - Core package including basics of spawning new
  actors, linking, monitoring, registering, and signaling.

* `memento-mori.timer` (aka `mori-timer`) - Timer and interval utilities.

* `memento-mori.server` (aka `mori-srv`) - Generic server with `defcall` and
  `defcast` macros for easily defining synchronous and asynchronous server
  requests, respectively.

* `memento-mori.supervisor` (aka `mori-sup`) - Process supervisor which can be
  used to build a supervision tree with configurable restart resistances
  and restart behaviors (one-for-one, one-for-all, etc).
