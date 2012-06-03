(cl:defpackage #:hipocrite.timer
  (:use #:cl #:hipocrite)
  (:export
   #:call-after
   #:call-interval
   #:cancel-timer
   #:send-after
   #:send-interval
   #:exit-after
   #:kill-after))
(cl:in-package #:hipocrite.timer)

(defun call-after (time function)
  (let ((timer (trivial-timers:make-timer function)))
    (trivial-timers:schedule-timer timer time)
    timer))

(defun call-interval (interval function)
  (let ((timer (trivial-timers:make-timer function)))
    (trivial-timers:schedule-timer timer 0 :repeat-interval interval)
    timer))

(defun cancel-timer (timer)
  (trivial-timers:unschedule-timer timer))

(defun send-after (time message &optional (actor (current-actor)))
  (call-after time (lambda () (send actor message))))

(defun send-interval (interval message &optional (actor (current-actor)))
  (call-interval interval (lambda () (send actor message))))

(defun exit-after (time reason &optional (actor (current-actor)))
  (call-after time (lambda () (exit reason actor))))

(defun kill-after (time &optional (actor (current-actor)))
  (call-after time (lambda () (kill actor))))
