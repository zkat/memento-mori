(cl:defpackage #:memento-mori.test.supervisor
  (:use #:cl #:alexandria))
(cl:in-package #:memento-mori.test.supervisor)

(defun test-sup ()
  (mori-sup:start-supervisor
   :name 'test-sup
   :debugp t
   :initial-child-specs
   (list
    (mori-sup:make-child-spec
     'hello-world-child
     (curry #'mori:spawn (lambda () (print "Hello, world!") (sleep 0.5))
            :linkp t)))))
