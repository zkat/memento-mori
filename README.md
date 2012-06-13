# Quickstart

`memento-mori` is
[hosted at Github](http://github.com/zkat/memento-mori). You can
download it there if it's not available on quicklisp yet.

```lisp
CL-USER> (ql:quickload 'memento-mori)
To load "memento-mori":
  Load 1 ASDF system:
    memento-mori
; Loading "memento-mori"
[package memento-mori]............................
[package memento-mori.timer]......................
[package memento-mori.server].....................
[package memento-mori.logger].
[memento-mori.logger:info] Starting mori-log server.
(MEMENTO-MORI)
CL-USER> (let ((actor (mori:spawn (lambda ()
                                    (mori-log:info "Message: ~s" (mori:receive))))))
           (mori:send actor "Hello, world!"))
[memento-mori.logger:info] Message: "Hello, world!"
T
CL-USER> (defstruct example-server)

EXAMPLE-SERVER
CL-USER> (mori-srv:defcall this-is-synchronous (this-is-an-argument)
             (server example-server)
           (mori-log:info "Server actor: ~s. Argument: ~s"
                          (mori:current-actor) this-is-an-argument)
           (values 1 2 3))
THIS-IS-SYNCHRONOUS
CL-USER> (let ((server (mori-srv:start #'make-example-server)))
           (mori:spawn (lambda ()
                         (mori-log:info "Caller: ~s." (mori:current-actor))
                         (mori-log:info "Multiple return values: ~a."
                                        (multiple-value-list
                                         (this-is-synchronous server 'an-argument)))
                         (mori:shutdown 'bye server))))
[memento-mori.logger:info] Caller: #<ACTOR [0 msgs] #x302000CEEFDD>.
[memento-mori.logger:info] Server actor: #<ACTOR [0 msgs] #x302000CEFB8D>. Argument: AN-ARGUMENT
[memento-mori.logger:info] Multiple return values: (1 2 3).
#<ACTOR [0 msgs] #x302000CEEFDD>
CL-USER>
```

# Introduction

### Remember you must die

`memento-mori` is a library for writing robust, actor-based systems in
Common Lisp. It draws inspiration from [Erlang/OTP](http://www.erlang.org),
particularly its crash-first approach to high-availability
systems. `memento-mori` works under the assumption that whatever you do,
you will eventually crash, and thus the best way to keep a system running
is to try to limit the damage and recover.

`memento-mori` includes utilities to easily build call-and-response servers
and supervision trees, along with the lower-level concurrency primitives
like actor spawning, messaging, and inter-actor linking and
monitoring. Running things across multiple processors is really just a
bonus.

This library is still under heavy development, and the API is not yet
documented. If you're curious enough, you can check out the example/
directory for som eye candy. Feel free to contact me with questions or
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

* `memento-mori.logger` (aka `mori-log`) - A simple log server that prints
  to `*error-output*` and `*debug-io*`.
