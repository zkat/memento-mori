;;;; hipocrite.asd

(asdf:defsystem #:hipocrite
  :serial t
  :description "Actor library inspired by Erlang's concurrency primitives."
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :license "MIT"
  :depends-on (alexandria bordeaux-threads #+sbcl sb-concurrency)
  :components
  ((:module src
            :serial t
            :components
            ((:file "package")
             (:file "mailbox")
             (:file "actor")))))

