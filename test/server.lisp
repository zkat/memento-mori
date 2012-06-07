(cl:defpackage #:hipocrite.test.server
  (:use #:cl #:alexandria)
  (:import-from #:hipocrite.server
                #:defcall
                #:defcast))
(cl:in-package #:hipocrite.test.server)

(defstruct example-server deferred-request deferred-value)

(defmethod hip-srv:on-init ((server example-server))
  (format t "~&Initializing ~a~%" server))

(defcall add-numbers (&rest numbers)
    (server example-server :server-form (hip:find-actor :test-server))
  (values (apply #'+ numbers) t))

(defcast be-happy (about)
    (server example-server)
  (format t "~&I, ~a (@~a), am SO HAPPY about ~s!~%"
          server (hip:current-actor) about))

(defcast please-die (why)
    (server example-server)
  (hip-srv:exit-server-loop why)
  (format t "~&Unreachable code. This won't print.~%"))

(defcall deferred-reply (value)
    (server example-server :request request)
  (setf (example-server-deferred-value server) value
        (example-server-deferred-request server) request)
  (hip-srv:defer-call-reply)
  (format t "~&Unreachable code. This won't print.~%"))

(defmethod hip-srv:on-message ((server example-server) message)
  (format t "~&Got a direct, non-call/cast message: ~s~%" message)
  (when-let ((request (example-server-deferred-request server))
             (value (example-server-deferred-value server)))
    (format t "~&Have something to reply to. Request: ~a, Value: ~a.~%"
            request value)
    (hip-srv:reply request value)
    (setf (example-server-deferred-value server) nil
          (example-server-deferred-request server) nil)))

(defmethod hip-srv:on-shutdown ((server example-server) reason)
  (format t "~&Cleanly shutting down ~a. Reason: ~a~%" server reason))

(defun test-example-server ()
  (let ((server (hip-srv:start #'make-example-server
                               :name :test-server
                               :debugp t)))
    (hip:spawn (lambda ()
                 (hip-timer:call-after 2 (lambda ()
                                           (hip:send server "This is a regular message.")))
                 (be-happy server pi)
                 (multiple-value-call
                     #'format t "~&Number: ~a, Second value: ~a.~%"
                     (add-numbers 1 2 3 4 5))
                 (format t "Deferred reply: ~a"
                         (deferred-reply server 'block-me))
                 (please-die server "I'm done with you."))
               :debugp t)))
