(cl:defpackage #:memento-mori.logger
  (:use #:cl #:alexandria #:memento-mori #:memento-mori.utils)
  (:nicknames #:mori-log)
  (:shadow #:warn #:error #:debug)
  (:export

   #:ensure-logger
   #:log-crash
   
   #:emergency
   #:alert
   #:critical
   #:error
   #:warn
   #:notice
   #:info
   #:debug))
(cl:in-package #:memento-mori.logger)

(defmacro defloglevel (level-name stream)
  `(defun ,level-name (format-string &rest format-args)
     (ignore-errors (apply #'log-message ,stream ',level-name format-string format-args))))

;;;
;;; API
;;;
(defstruct logger)

(defun ensure-logger ()
  (handler-case
      (mori-srv:start #'make-logger :name 'logger)
    (actor-already-exists ()
      (find-actor 'logger))))

;; Levels taken from man syslog
(defloglevel emergency *error-output*)
(defloglevel alert *error-output*)
(defloglevel critical *error-output*)
(defloglevel error *error-output*)
(defloglevel warn *error-output*)
(defloglevel notice *debug-io*)
(defloglevel info *debug-io*)
(defloglevel debug *debug-io*)

;; Really meant for internal use of the core actor package.
(defun log-crash (actor exit)
  (typecase exit
    (actor-shutdown nil)
    (actor-completion nil)
    (actor-kill (warn "Actor ~a killed." actor))
    (actor-error (error "Actor ~a shutting down due to error: ~a"
                        actor (actor-exit-reason exit)))
    (actor-exit (warn "Actor ~a exited abnormally: ~a"
                      actor (actor-exit-reason exit)))))

;;;
;;; Server protocol implementation
;;;
(defmethod mori-srv:on-init ((logger logger))
  (log-message *debug-io* 'info "Starting mori-log server."))

(defmethod mori-srv:on-shutdown ((logger logger) reason)
  (cl:warn "memento-mori logger server is shutting down because of ~a!" reason))

(mori-srv:defcast log-message (stream log-level format-string &rest format-args)
    (logger logger :server-form 'logger)
  (let (*print-pretty*)
    (format stream
            "~&[~(~s~)] ~a~%"
            log-level
            (apply #'format nil format-string format-args)))
  (finish-output stream))

;; Start the logger on load.
(eval-when (:load-toplevel :execute)
  (ensure-logger))
