(cl:defpackage #:hipocrite.test.server
  (:use #:cl #:alexandria)
  (:import-from #:hipocrite.server
                #:defcall
                #:defcast))
(cl:in-package #:hipocrite.test.server)

(defstruct example-server)

(defcall add-numbers (&rest numbers)
    (server example-server :server-form (hip:find-actor :test-server))
  (values (apply #'+ numbers) t))

(defcast be-happy (about)
    (server example-server)
  (format t "~&I, the example-server (@~a), am SO HAPPY about ~s!~%"
          (hip:current-actor) about))

(defcast please-die (why)
    (server example-server)
  (hip-srv:exit-server-loop why))

(defmethod hip-srv:on-direct-message ((server example-server) message)
  (format t "~&Got a direct, non-call/cast message: ~s~%" message))

(defmethod hip-srv:on-shutdown ((server example-server) reason)
  (format t "~&Cleanly shutting down ~a. Reason: ~a~%" server reason))

(defun test-example-server ()
  (let ((server (hip-srv:start #'make-example-server
                               :name :test-server
                               :debugp t)))
    (hip:spawn (lambda ()
                 (be-happy server pi)
                 (hip:send server "This is a regular message.")
                 (multiple-value-call
                     #'format t "~&Number: ~a, Second value: ~a.~%"
                     (add-numbers 1 2 3 4 5))
                 (please-die server "I'm done with you."))
               :debugp t)))
