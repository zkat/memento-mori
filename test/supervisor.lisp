(cl:defpackage #:hibernate.test.supervisor
  (:use #:cl #:alexandria))
(cl:in-package #:hibernate.test.supervisor)

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
