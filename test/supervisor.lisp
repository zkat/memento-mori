(cl:defpackage #:hipocrite.test.supervisor
  (:use #:cl #:alexandria))
(cl:in-package #:hipocrite.test.supervisor)

(defun test-sup ()
  (hip-sup:start-supervisor
   :name 'test-sup
   :debugp t
   :initial-child-specs
   (list
    (hip-sup:make-child-spec
     'hello-world-child
     (curry #'hip:spawn (lambda () (print "Hello, world!") (sleep 0.5))
            :linkp t)))))
