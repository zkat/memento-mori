;;;; package.lisp

(defpackage #:hipocrite
  (:use #:cl #:alexandria)
  (:export #:spawn
           #:link
           #:unlink
           #:register
           #:unregister
           #:find-actor
           #:exit
           #:kill
           #:actor-exit-actor
           #:actor-exit-reason
           #:current-actor
           #:actor-alive-p
           #:send
           #:receive
           #:receive-if
           #:receive-cond))
