# Quickstart

`hipocrite` is [hosted at Github](http://github.com/zkat/hipocrite).

```lisp
CL-USER> (ql:quickload 'hipocrite)
To load "hipocrite":
  Load 1 ASDF system:
    hipocrite
; Loading "hipocrite"

(HIPOCRITE)
CL-USER> (let ((actor (hip:spawn (lambda () (print (hip:receive))))))
           (hip:send actor "Hello, world!"))

"Hello, world!" T
CL-USER> (defstruct example-server)
EXAMPLE-SERVER
CL-USER> (hip-srv:defcall this-is-synchronous (this-is-an-argument)
             (server example-server)
           (format t "~&Server actor: ~s. Argument: ~s~%"
                   (hip:current-actor) this-is-an-argument)
           'a-return-value)

THIS-IS-SYNCHRONOUS
CL-USER> (LET ((SErver (hip-srv:start #'make-example-server)))
           (hip:spawn (lambda ()
                        (format t "~&Caller: ~s.~%" (hip:current-actor))
                        (print
                         (this-is-synchronous server 'an-argument))
                        (hip:kill server))))
Caller: #<ACTOR [0 msgs] #x302000EA248D>.
Server actor: #<ACTOR [0 msgs] #x302000EA369D>. Argument: AN-ARGUMENT

A-RETURN-VALUE
#<ACTOR [0 msgs] #x302000EA248D>
NIL
```

# Introduction

`hipocrite` is a library for writing robust, agent-based systems in Common
Lisp. It draws inspiration from [Erlang/OTP](http://www.erlang.org),
particularly its generic behaviors. `hipocrite` includes utilities to
easily build call-and-response servers and supervision trees, along with
the lower-level concurrency primitives like actor spawning, messaging, and
inter-actor linking and monitoring. The concurrency is really just a
bonus. :)

The library is still under heavy development, and the API is not yet
documented. If you're curious enough, you can check out the test/ directory
for some interesting examples. Feel free to contact me with questions or
ideas.

# Packages

* `hipocrite` (aka `hip`) - Core package including basics of spawning new
  actors, linking, monitoring, registering, and signaling.

* `hipocrite.timer` (aka `hip-timer`) - Timer and interval utilities.

* `hipocrite.server` (aka `hip-srv`) - Generic server with `defcall` and
  `defcast` macros for easily defining synchronous and asynchronous server
  requests, respectively.

* `hipocrite.supervisor` (aka `hip-sup`) - Process supervisor which can be
  used to build a supervision tree with configurable restart resistances
  and restart behaviors (one-for-one, one-for-all, etc).

# 'hipocrite'?

From [Wikipedia](https://en.wikipedia.org/wiki/Actor):

```
An actor (sometimes actress for female; see terminology) is a person
who acts in a dramatic or comic production and who works in film,
television, theatre, or radio in that capacity.[1] The ancient Greek word
for an "actor," ὑποκριτής (hypokrites), means literally "one who
interprets";[2] in this sense, an actor is one who interprets a dramatic
character.[3]

```
