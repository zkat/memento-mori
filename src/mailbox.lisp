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
  (without-interrupts
    (if timeout
        (multiple-value-bind (value completedp)
            (with-timeout timeout
              (#+sbcl sb-sys:allow-with-interrupts
               #-sbcl progn
               (with-interrupts (do-receive mailbox))))
          (cond (completedp value)
                (on-timeout (funcall on-timeout))
                (t (error 'receive-timeout))))
        (#+sbcl sb-sys:allow-with-interrupts
         #-sbcl progn
         (with-interrupts (do-receive mailbox))))))

;; Oy vey, how to do this while still being interrupt-safe? :\
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
          (#+sbcl sb-sys:allow-with-interrupts
           #-sbcl progn
           (with-interrupts (do-selective-receive mailbox test)))
        (funcall callback value))))

(defun do-selective-receive (mailbox test)
  (without-interrupts
    (bt:with-lock-held ((mailbox-lock mailbox))
      (let ((q (mailbox-queue mailbox))
            match
            callback)
        (loop until callback
           do (let (rest)
                (unwind-protect
                     (loop for (item . current-rest) on (dequeue-all q)
                        for maybe-callback = (#+sbcl sb-sys:allow-with-interrupts
                                              #-sbcl progn
                                              (with-interrupts (funcall test item)))
                        do (setf rest current-rest)
                        when maybe-callback
                        do (setf match item
                                 callback maybe-callback)
                          (return)
                        else
                        do (enqueue item q))
                  (mapcar (rcurry #'enqueue q) rest)))
           unless callback
           do (#+sbcl sb-sys:allow-with-interrupts
               #-sbcl progn
               (with-interrupts
                 (bt:condition-wait (mailbox-cond-var mailbox) (mailbox-lock mailbox))))
           else
           do (return-from do-selective-receive (values match callback)))))))

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
