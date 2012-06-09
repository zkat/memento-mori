(cl:defpackage #:memento-mori.logger
  (:use #:cl #:alexandria #:memento-mori #:memento-mori.utils)
  (:nicknames #:mori-log)
  (:shadow #:warn #:error)
  (:export

   #:ensure-logger
   
   #:emergency
   #:alert
   #:critical
   #:error
   #:warn
   #:notice
   #:info
   #:debug))
(cl:in-package #:memento-mori.logger)

(defstruct logger)

(defmethod mori-srv:on-init ((logger logger))
  (log-message *debug-io* 'info "Starting mori-log server."))

(defmethod mori-srv:on-shutdown ((logger logger) reason)
  (warn "memento-mori logger server is shutting down because of ~a!" reason))

(defun log-message (stream log-level format-string &rest format-args)
  (format stream
          "~&[~s] ~a~%"
          log-level
          (apply #'format nil format-string format-args))
  (finish-output stream))

(defmacro defloglevel (level-name stream)
  `(mori-srv:defcast ,level-name (format-string &rest format-args)
       (logger logger :server-form (mori:find-actor 'logger))
     (ignore-errors (apply #'log-message ,stream ',level-name format-string format-args))))

;; Levels taken from man syslog
(defloglevel emergency *error-output*)
(defloglevel alert *error-output*)
(defloglevel critical *error-output*)
(defloglevel error *error-output*)
(defloglevel warn *error-output*)
(defloglevel notice *debug-io*)
(defloglevel info *debug-io*)
(defloglevel debug *debug-io*)

(defun start-logger ()
  (mori-srv:start #'make-logger
                  :name 'logger))

(defun ensure-logger ()
  (handler-case
      (start-logger)
    (actor-already-exists ()
      (find-actor 'logger))))

(eval-when (:load-toplevel :execute)
  ;; TODO - no, this is not thread safe, but it'll work out once actor
  ;;        registration is hashed out better.
  (ensure-logger))
