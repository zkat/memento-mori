(cl:defpackage #:memento-mori.example.server
  (:use #:cl #:alexandria))
(cl:in-package #:memento-mori.example.server)

(defstruct example-server)

(defmethod mori:on-init ((server example-server))
  (format t "~&Initializing ~a, with actor ~a~%" server (mori:current-actor)))

(defmethod mori:on-message ((server example-server) msg)
  (format t "~&~a got a message: ~s~%" server msg)
  (when (eq 'hello-world! msg)
    (mori:receive-cond (msg)
      ((eq msg 'goodbye!)
       (format t "~&Got another message: ~s~%" msg)))))

(defmethod mori:on-shutdown ((server example-server) reason)
  (format t "~&~a shutting down because of ~a~%" server reason))

(defun test-server (scheduler)
  (let ((server (mori:spawn (make-example-server) :scheduler scheduler)))
    (mori:send server 'hello-world!)
    (mori:send server 'test1)
    (mori:send server 'test2)
    (mori:send server 'test3)
    (mori:send server 'goodbye!)
    (sleep 6)
    (mori:exit 'bye server)))
