(cl:defpackage #:hipocrite.test.server
  (:use #:cl #:alexandria #:hipocrite)
  (:import-from #:hipocrite.server
                #:start
                #:defcall
                #:call))
(cl:in-package #:hipocrite.test.server)

(defclass arithmetic-server () ())

(defcall + (server arithmetic-server) (&rest numbers)
  (values (apply #'+ numbers) t))

(defun test-arithmetic-server ()
  (let ((actor (start (make-instance 'arithmetic-server)
                      :name 'test-server)))
    (spawn (lambda ()
             (multiple-value-call
                 #'format t "~&Number: ~a, Second value: ~a~%"
                 (call actor '+ '(1 2 3 4 5)))
             (kill 'die actor))
           :debugp t)))
