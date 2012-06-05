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
CL-USER> (hip:spawn (lambda ()
                      (loop for i below 10
                           do (hip:spawn (lambda (&aux (i i))
                                           (sleep (random 5 (make-random-state t))) i)
                                         :linkp t))
                      (loop for exit = (hip:receive :timeout 8)
                         while (hip:link-exit-p exit)
                         do (format t "~&Got an exit of type ~a. Reason: ~s~%"
                                    (hip:link-exit-type exit)
                                    (hip:link-exit-reason exit)))
                      (format t "~&Done. Exiting master actor.~%"))
                    :trap-exits-p t)
Got an exit of type ACTOR-COMPLETION. Reason: 3
Got an exit of type ACTOR-COMPLETION. Reason: 5
Got an exit of type ACTOR-COMPLETION. Reason: 6
Got an exit of type ACTOR-COMPLETION. Reason: 1
Got an exit of type ACTOR-COMPLETION. Reason: 9
Got an exit of type ACTOR-COMPLETION. Reason: 4
Got an exit of type ACTOR-COMPLETION. Reason: 8
Got an exit of type ACTOR-COMPLETION. Reason: 2
Got an exit of type ACTOR-COMPLETION. Reason: 7
Got an exit of type ACTOR-COMPLETION. Reason: 10
Done. Exiting master actor.
#<ACTOR [0 msgs] #x302000D08C4D>
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
