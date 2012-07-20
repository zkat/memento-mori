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

(MEMENTO-MORI)
CL-USER> (loop repeat 6 do (mori:start-event-thread))
NIL
CL-USER> (mori:send (mori:spawn #'print) "Hello, world!")

"Hello, world!" "Hello, world!"
CL-USER>     

```

# Introduction

### Remember you must die

`memento-mori` is a library for writing robust, actor-based systems in
Common Lisp. It draws inspiration from [Erlang/OTP](http://www.erlang.org),
particularly its crash-first approach to high-availability systems, as well
as [Akka](http://akka.io). `memento-mori` works under the assumption that
whatever you do, you will eventually crash, and thus the best way to keep a
system running is to try to limit the damage and recover.
