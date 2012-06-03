;;;; package.lisp

(defpackage #:hipocrite
  (:use #:cl #:alexandria #:hipocrite.utils)
  (:export
   #:*debug-on-error-p*
   #:spawn
   #:link
   #:unlink
   #:monitor
   #:demonitor
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
