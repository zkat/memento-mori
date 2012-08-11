;;;; memento-mori.asd

(asdf:defsystem #:memento-mori
  :serial t
  :description "Event-based actor system."
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :license "MIT"
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:trivial-timers)
  :components
  ((:module src
            :serial t
            :components
            ((:file "utils")
             (:file "queue")
             (:file "memento-mori")))))
