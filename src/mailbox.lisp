(cl:defpackage #:memento-mori.mailbox
  (:use #:cl #:memento-mori.utils)
  (:export #:make-mailbox
           #:mailbox-count
           #:send
           #:receive-timeout
           #:receive
           #:receive-cond))
(in-package #:memento-mori.mailbox)

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
  obj)

(define-condition receive-timeout (error) ())
(defun receive (mailbox &key timeout on-timeout)
  (if timeout
      (multiple-value-bind (value completedp)
          (with-timeout timeout
            (do-receive mailbox))
        (cond (completedp value)
              (on-timeout (funcall on-timeout))
              (t (error 'receive-timeout))))
      (do-receive mailbox)))

;; Oy vey, how to do this while still being interrupt-safe? :\
(defun do-receive (mailbox)
  (bt:with-lock-held ((mailbox-lock mailbox))
    (loop when (queue-empty-p (mailbox-queue mailbox))
       do (bt:condition-wait (mailbox-cond-var mailbox) (mailbox-lock mailbox))
       unless (queue-empty-p (mailbox-queue mailbox))
       do (return-from do-receive (dequeue (mailbox-queue mailbox))))))

(defun receive-choices (mailbox choices &key timeout on-timeout)
  ;; TODO - This m-v-l/values-list bullshit is too much. Please don't do it.
  (if timeout
      (multiple-value-bind (value completedp)
          (with-timeout timeout
            (multiple-value-list
             (do-selective-receive mailbox choices)))
        (cond (completedp (values-list value))
              (on-timeout (funcall on-timeout))
              (t (error 'receive-timeout))))
      (do-selective-receive mailbox choices)))

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
         do (return-from do-selective-receive (funcall callback value))))))

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
