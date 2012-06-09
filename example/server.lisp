(cl:defpackage #:memento-mori.example.server
  (:use #:cl #:alexandria)
  (:import-from #:memento-mori.server
                #:defcall
                #:defcast))
(cl:in-package #:memento-mori.example.server)

(defstruct example-server deferred-request deferred-value)

(defmethod mori-srv:on-init ((server example-server))
  (format t "~&Initializing ~a~%" server))

(defcall add-numbers (&rest numbers)
    (server example-server :server-form (mori:find-actor :test-server))
  (values (apply #'+ numbers) t))

(defcast be-happy (about)
    (server example-server)
  (format t "~&I, ~a (@~a), am SO HAPPY about ~s!~%"
          server (mori:current-actor) about))

(defcast please-die (why)
    (server example-server)
  (mori-srv:exit-server-loop why)
  (format t "~&Unreachable code. This won't print.~%"))

(defcall deferred-reply (value)
    (server example-server :request request)
  (setf (example-server-deferred-value server) value
        (example-server-deferred-request server) request)
  (mori-srv:defer-call-reply)
  (format t "~&Unreachable code. This won't print.~%"))

(defmethod mori-srv:on-message ((server example-server) message)
  (format t "~&Got a direct, non-call/cast message: ~s~%" message)
  (when-let ((request (example-server-deferred-request server))
             (value (example-server-deferred-value server)))
    (format t "~&Have something to reply to. Request: ~a, Value: ~a.~%"
            request value)
    (mori-srv:reply request value 1 2 3)
    (setf (example-server-deferred-value server) nil
          (example-server-deferred-request server) nil)))

(defmethod mori-srv:on-shutdown ((server example-server) reason)
  (format t "~&~a is shutting down because of ~a.~%" server reason))

(defun test-example-server ()
  (let ((server (mori-srv:start #'make-example-server
                               :name :test-server
                               :debugp t)))
    (mori:spawn (lambda ()
                 (mori-timer:call-after 2 (lambda ()
                                           (mori:send server "This is a regular message.")))
                 (be-happy server pi)
                 (multiple-value-call
                     #'format t "~&Number: ~a, Second value: ~a.~%"
                     (add-numbers 1 2 3 4 5))
                 (format t "Deferred reply: ~a"
                         (multiple-value-list (deferred-reply server 'block-me)))
                 (please-die server "I'm done with you."))
               :debugp t)))
