;;;; memento-mori.asd

(asdf:defsystem #:memento-mori
  :serial t
  :description "Robustness through actors."
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
             (:file "server")
             (:file "logger")))))
