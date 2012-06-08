(cl:defpackage #:hipocrite.test.supervisor
  (:use #:cl #:alexandria))
(cl:in-package #:hipocrite.test.supervisor)

(defun test-sup ()
  (hip-sup:start-supervisor
   'hip-sup:one-for-one :name 'test-sup :debugp t
   :initial-child-specs
   (list
    (hip-sup:make-child-spec
     'test
     (lambda ()
       (hip:spawn (lambda () (print "Hello, world!") (sleep 10))
                  :linkp t))))))
