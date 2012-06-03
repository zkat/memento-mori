(cl:defpackage #:hipocrite.mailbox
  (:use #:cl #:hipocrite.utils)
  (:export #:make-mailbox
           #:mailbox-count
           #:send
           #:receive
           #:receive-cond))
(in-package #:hipocrite.mailbox)

;; Mailbox
(defstruct mailbox
  (cond-var (bt:make-condition-variable))
  (lock (bt:make-lock))
  (queue (make-queue)))

(defmethod print-object ((mailbox mailbox) stream)
  (print-unreadable-object (mailbox stream :type t :identity t)
    (format stream "[~D messages]" (mailbox-count mailbox))))

(defun mailbox-count (mailbox)
  (bt:with-lock-held ((mailbox-lock mailbox))
    (length (car (mailbox-queue mailbox)))))

(defun send (mailbox obj)
  (bt:with-lock-held ((mailbox-lock mailbox))
    (enqueue obj (mailbox-queue mailbox))
    (bt:condition-notify (mailbox-cond-var mailbox)))
  t)

(defun receive (mailbox &key timeout on-timeout)
  (if timeout
      (multiple-value-bind (value completedp)
          (with-timeout timeout
            (do-receive mailbox))
        (if completedp
            (values value t)
            (values (when on-timeout
                      (funcall on-timeout))
                  nil)))
      (values (do-receive mailbox) t)))

;; Oy vey, how to do this while still being interrupt-safe? :\
(defun do-receive (mailbox)
  (bt:with-lock-held ((mailbox-lock mailbox))
    (loop when (queue-empty-p (mailbox-queue mailbox))
       do (bt:condition-wait (mailbox-cond-var mailbox) (mailbox-lock mailbox))
       unless (queue-empty-p (mailbox-queue mailbox))
       do (return-from do-receive (dequeue (mailbox-queue mailbox))))))

(defun receive-choices (mailbox choices &key timeout on-timeout)
  (if timeout
      (multiple-value-bind (value completedp)
          (with-timeout timeout
            (do-selective-receive mailbox choices))
        (if completedp
            (values value t)
            (values (when on-timeout
                      (funcall on-timeout))
                    nil)))
      (values (do-selective-receive mailbox choices))))

(defun do-selective-receive (mailbox choices)
  ;; TODO - Ugly and stupid, but I just need a working prototype for now.
  (bt:with-lock-held ((mailbox-lock mailbox))
    (let ((q (mailbox-queue mailbox))
          got-value-p
          value
          callback)
      (loop until got-value-p
         do (loop for (test . on-success) in choices
               do (loop for item in (dequeue-all q)
                     do (if (and (null got-value-p)
                                 (funcall test item))
                            (setf got-value-p t
                                  value item
                                  callback on-success)
                            (enqueue item q))))
         if (null got-value-p)
         do (bt:condition-wait (mailbox-cond-var mailbox) (mailbox-lock mailbox))
         else
         do (return (funcall callback value))))))

(defmacro receive-cond ((value-var mailbox &key timeout on-timeout) &body clauses)
  `(receive-choices ,mailbox
                    (list
                     ,@(loop for (test . forms) in clauses
                          collect `(cons (lambda (,value-var)
                                           (declare (ignorable ,value-var))
                                           ,test)
                                         (lambda (,value-var)
                                           (declare (ignorable ,value-var))
                                           ,@forms))))
                    :timeout ,timeout :on-timeout ,(when on-timeout
                                                     `(lambda () ,on-timeout))))
