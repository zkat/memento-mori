;;;; hipocrite.asd

(asdf:defsystem #:hipocrite
  :serial t
  :description "Actor library inspired by Erlang's concurrency primitives."
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :license "MIT"
  :depends-on (alexandria bordeaux-threads trivial-timers)
  :components
  ((:module src
            :serial t
            :components
            ((:file "utils")
             (:file "mailbox")
             (:file "actor")
             (:file "timer")
             (:file "server")))))
