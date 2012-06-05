(cl:defpackage #:hipocrite.test.server
  (:use #:cl #:alexandria #:hipocrite)
  (:import-from #:hipocrite.server
                #:start
                #:defcall
                #:call
                #:defcast
                #:cast
                #:on-direct-message))
(cl:in-package #:hipocrite.test.server)

(defclass arithmetic-server () ())

(defcall add-numbers (server arithmetic-server) (&rest numbers)
  (values (apply #'+ numbers) t))

(defcast be-happy (server arithmetic-server) (about)
  (format t "~&I, the arithmetic-server (@~a), am SO HAPPY about ~s!~%"
          (current-actor) about))

(defmethod on-direct-message ((server arithmetic-server) message)
  (format t "~&Got a direct, non-call/cast message: ~s~%" message))

(defun test-arithmetic-server ()
  (let ((server (start (make-instance 'arithmetic-server)
                      :name :test-server
                      :debugp t)))
    (spawn (lambda ()
             (be-happy server pi)
             (send server "This is a regular message.")
             (multiple-value-call
                 #'format t "~&Number: ~a, Second value: ~a.~%"
                 (add-numbers server 1 2 3 4 5))
             (kill 'die server))
           :debugp t)))
