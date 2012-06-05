(cl:defpackage #:hipocrite.test.server
  (:use #:cl #:alexandria)
  (:import-from #:hipocrite.server
                #:defcall
                #:defcast))
(cl:in-package #:hipocrite.test.server)

(defclass arithmetic-server () ())
(defun make-arithmetic-server ()
  (make-instance 'arithmetic-server))

(defcall add-numbers (&rest numbers)
    (server arithmetic-server :server-form (hip:find-actor :test-server))
  (values (apply #'+ numbers) t))

(defcast be-happy (about)
    (server arithmetic-server)
  (format t "~&I, the arithmetic-server (@~a), am SO HAPPY about ~s!~%"
          (hip:current-actor) about))

(defmethod hip-srv:on-direct-message ((server arithmetic-server) message)
  (format t "~&Got a direct, non-call/cast message: ~s~%" message))

(defun test-arithmetic-server ()
  (let ((server (hip-srv:start #'make-arithmetic-server
                               :name :test-server
                               :debugp t)))
    (hip:spawn (lambda ()
                 (be-happy server pi)
                 (hip:send server "This is a regular message.")
                 (multiple-value-call
                     #'format t "~&Number: ~a, Second value: ~a.~%"
                     (add-numbers 1 2 3 4 5))
                 (hip:kill server))
               :debugp t)))
