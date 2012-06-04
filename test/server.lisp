(cl:defpackage #:hipocrite.test.server
  (:use #:cl #:alexandria #:hipocrite)
  (:import-from #:hipocrite.server
                #:start
                #:defcall
                #:call
                #:defcast
                #:cast))
(cl:in-package #:hipocrite.test.server)

(defclass arithmetic-server () ())

(defcall + (server arithmetic-server) (&rest numbers)
  (values (apply #'+ numbers) t))

(defcast be-happy (server arithmetic-server) (about)
  (format t "~&I, the arithmetic-server (@~a), am SO HAPPY about ~s!~%"
          (current-actor) about))

(defun test-arithmetic-server ()
  (let ((actor (start (make-instance 'arithmetic-server)
                      :name 'test-server)))
    (spawn (lambda ()
             (cast actor 'be-happy '(pie))
             (multiple-value-call
                 #'format t "~&Number: ~a, Second value: ~a.~%"
                 (call actor '+ '(1 2 3 4 5)))
             (kill 'die actor))
           :debugp t)))
