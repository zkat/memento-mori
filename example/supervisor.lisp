(cl:defpackage #:memento-mori.example.supervisor
  (:use #:cl #:alexandria))
(cl:in-package #:memento-mori.example.supervisor)

(defun test-sup ()
  (mori-sup:start-supervisor
   :name 'test-sup
   :initial-child-specs
   (list
    (mori-sup:make-child-spec
     'hello-world-child
     (curry #'mori:spawn (lambda () (print "Hello, world!") (sleep 0.5))
            :linkp t)))))
