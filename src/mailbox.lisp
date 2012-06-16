(cl:defpackage #:memento-mori.mailbox
  (:use #:cl #:memento-mori.utils)
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

(defun selective-receive (mailbox test &key timeout on-timeout)
  (if timeout
      (multiple-value-bind (value callback)
          (block timer-block
            (let ((timer (trivial-timers:make-timer
                          (lambda ()
                            (return-from timer-block (values nil nil))))))
              (trivial-timers:schedule-timer timer timeout)
              (unwind-protect
                   (do-selective-receive mailbox test)
                (trivial-timers:unschedule-timer timer))))
        (cond (callback (funcall callback value))
              (on-timeout (funcall on-timeout))
              (t (error 'receive-timeout))))
      (multiple-value-bind (value callback)
          (do-selective-receive mailbox test)
        (funcall callback value))))

(defun do-selective-receive (mailbox test)
  ;; TODO - interrupts might mess with the mailbox contents. An error in
  ;;        (funcall test item) can also throw the mailbox out of sync. Put
  ;;        unwind-protects and with-/without-interrupts in the appropriate
  ;;        places, here.
  (bt:with-lock-held ((mailbox-lock mailbox))
    (let ((q (mailbox-queue mailbox))
          match
          callback)
      (loop until callback
         do (loop for item in (dequeue-all q)
               for maybe-callback = (with-interrupts (funcall test item))
               when maybe-callback
               do (setf match item
                        callback maybe-callback)
                 (return)
               else
               do (enqueue item q))
         unless callback
         do (bt:condition-wait (mailbox-cond-var mailbox) (mailbox-lock mailbox))
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
