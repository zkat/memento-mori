(cl:defpackage #:hipocrite.gen-server
  (:use #:cl #:hipocrite)
  (:export
   ;; API
   #:start
   ;; gen-server callbacks
   #:init
   #:handle-call
   #:handle-cast
   #:handle-info
   #:terminate))
(cl:in-package #:hipocrite.gen-server)

(defgeneric init (driver))
(defgeneric handle-call (driver request from))
(defgeneric handle-cast (driver request))
(defgeneric handle-info (driver info))
(defgeneric terminate (driver reason))

(defparameter +cast-marker+ (gensym "CAST"))
(defparameter +call-marker+ (gensym "CALL"))

(defun start (driver &key linkp monitorp trap-exits-p
              (debugp *debug-on-error-p*))
  (spawn (lambda (&aux terminatedp)
           (init driver)
           (handler-bind ((actor-exit (lambda (exit)
                                        (unless terminatedp
                                          (terminate driver exit))))
                          (error (lambda (e)
                                   (unless terminatedp
                                     (terminate driver e)))))
             (loop for msg = (receive)
                do (cond ((call-message-p msg)
                          (handle-call driver (call-request msg) (call-from msg)))
                         ((cast-message-p msg)
                          (handle-cast driver (cast-request msg)))
                         (t
                          (handle-info driver msg))))
             (setf terminatedp t)
             ;; WHY IS THIS UNREACHABLE?
             (terminate driver (make-condition 'actor-exit
                                               :actor (current-actor)
                                               :reason :normal))))
         :linkp linkp
         :monitorp monitorp
         :trap-exits-p trap-exits-p
         :debugp debugp))

(defun call-message-p (msg)
  (and (listp msg) (eq +call-marker+ (car msg))))

(defun call-request (msg)
  (second msg))

(defun call-from (msg)
  (third msg))

(defun cast-message-p (msg)
  (and (listp msg) (eq +cast-marker+ (car msg))))

(defun cast-request (msg)
  (second msg))
