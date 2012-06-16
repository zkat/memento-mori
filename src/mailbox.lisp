(cl:defpackage #:memento-mori.mailbox
  (:use #:cl #:alexandria #:memento-mori.utils)
  (:export #:make-mailbox
           #:mailbox-count
           #:send
           #:receive-timeout
           #:receive
           #:selective-receive
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
  (without-interrupts
    (bt:with-lock-held ((mailbox-lock mailbox))
      (enqueue obj (mailbox-queue mailbox))
      (bt:condition-notify (mailbox-cond-var mailbox))))
  obj)

(define-condition receive-timeout (error) ())

(defun receive (mailbox &key timeout on-timeout)
  (if timeout
      (multiple-value-bind (value completedp)
          ;; NOTE - Unlike the timeout in selective-receive, this one is
          ;; relatively benign because we only ever interrupt during a
          ;; condition-wait, with everything else being safe and internal.
          (with-timeout timeout
            (do-receive mailbox))
        (cond (completedp value)
              (on-timeout (funcall on-timeout))
              (t (error 'receive-timeout))))
      (do-receive mailbox)))

(defun do-receive (mailbox)
  (without-interrupts
    (bt:with-lock-held ((mailbox-lock mailbox))
      (loop when (queue-empty-p (mailbox-queue mailbox))
         do (#+sbcl sb-sys:with-interrupts
             #-sbcl progn
             (with-interrupts
               (bt:condition-wait (mailbox-cond-var mailbox) (mailbox-lock mailbox))))
         unless (queue-empty-p (mailbox-queue mailbox))
         do (return-from do-receive (dequeue (mailbox-queue mailbox)))))))

(defun selective-receive (mailbox test &key timeout on-timeout)
  (if timeout
      (multiple-value-bind (value callback)
          (let* ((timed-out-p-box (make-array nil :initial-contents nil))
                 (timer (trivial-timers:make-timer
                         (lambda ()
                           (bt:with-lock-held ((mailbox-lock mailbox))
                             (setf (aref timed-out-p-box) t)
                             (bt:condition-notify (mailbox-cond-var mailbox)))))))
            (trivial-timers:schedule-timer timer timeout)
            (unwind-protect
                 (do-selective-receive mailbox test timed-out-p-box)
              (trivial-timers:unschedule-timer timer)))
        (cond (callback (funcall callback value))
              (on-timeout (funcall on-timeout))
              (t (error 'receive-timeout))))
      (multiple-value-bind (value callback)
          (do-selective-receive mailbox test (make-array nil :initial-contents nil))
        (funcall callback value))))

(defun do-selective-receive (mailbox test timed-out-p-box)
  (bt:with-lock-held ((mailbox-lock mailbox))
    (let ((q (mailbox-queue mailbox))
          match
          callback)
      (loop until callback
         do
           (let (item itemp rest)
             (unwind-protect
                  (loop with maybe-callback
                     for (current-item . current-rest) on (dequeue-all q)
                     do (setf rest current-rest
                              item current-item
                              itemp t
                              maybe-callback (funcall test item))
                     when maybe-callback
                     do (setf match current-item
                              callback maybe-callback
                              itemp nil)
                       (return)
                     else
                     do (enqueue item q))
               (when (and itemp (null callback))
                 (enqueue item q))
               (mapcar (rcurry #'enqueue q) rest)))
         unless callback
         do
           ;; NOTE - I'm not sure this one actually ever does its thing.
           (when (aref timed-out-p-box)
             (return-from do-selective-receive
               (values nil nil)))
           (bt:condition-wait (mailbox-cond-var mailbox) (mailbox-lock mailbox))
           (when (aref timed-out-p-box)
             (return-from do-selective-receive
               (values nil nil)))
         else
         do (return-from do-selective-receive (values match callback))))))

(defmacro receive-cond ((value-var mailbox &key timeout on-timeout) &body clauses)
  `(selective-receive ,mailbox
                      (lambda (,value-var)
                        (cond
                          ,@(loop for (test . forms) in clauses
                               collect `(,test (lambda (,value-var)
                                                 (declare (ignorable ,value-var))
                                                 ,@forms)))))
                      :timeout ,timeout :on-timeout ,(when on-timeout
                                                           `(lambda () ,on-timeout))))
