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
CL-USER> (hip-srv:defcall print-current-actor (and-this)
             (server example-server)
           (format t "~&Server actor: ~s. Argument: ~s~%"
                   (hip:current-actor) and-this)
           'a-return-value)

PRINT-CURRENT-ACTOR
CL-USER> (let ((server (hip-srv:start #'make-example-server)))
           (hip:spawn (lambda ()
                        (format t "~&Caller: ~s.~%" (hip:current-actor))
                        (print
                         (print-current-actor server 'an-argument))
                        (hip:kill server))))
Caller: #<ACTOR [0 msgs] #x302000EA248D>.
Server actor: #<ACTOR [0 msgs] #x302000EA369D>. Argument: AN-ARGUMENT

A-RETURN-VALUE #<ACTOR [0 msgs] #x302000EA248D>
NIL
```

# Introduction

`hipocrite` is a concurrency/parallelism utility library for Common Lisp. Its API is based on
[Erlang](http://www.erlang.org)'s concurrency core. Like Erlang, `hipocrite` hopes to focus more on
robustness and system architecture than simply spreading the load across processors (although it
does that, too). For this reason, `hipocrite` includes utilities to easily build call-and-response
servers and supervision trees, along with the lower-level concurrency primitives like actor
spawning, messaging, and inter-actor linking and monitoring.

# 'hipocrite'?

From [Wikipedia](https://en.wikipedia.org/wiki/Actor):

```
An actor (sometimes actress for female; see terminology) is a person who acts in a dramatic or comic
production and who works in film, television, theatre, or radio in that capacity.[1] The ancient
Greek word for an "actor," ὑποκριτής (hypokrites), means literally "one who interprets";[2] in this
sense, an actor is one who interprets a dramatic character.[3]

```
