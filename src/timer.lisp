(cl:defpackage #:memento-mori.timer
  (:use #:cl #:memento-mori)
  (:nicknames #:mori-timer)
  (:export
   #:call-after
   #:call-interval
   #:cancel-timer
   #:send-after
   #:send-interval
   #:exit-after
   #:kill-after))
(cl:in-package #:memento-mori.timer)

(defun call-after (time function)
  (let ((timer (trivial-timers:make-timer function)))
    (trivial-timers:schedule-timer timer time)
    timer))

(defun call-interval (interval function &key (delay 0))
  (let ((timer (trivial-timers:make-timer function)))
    (trivial-timers:schedule-timer timer delay :repeat-interval interval)
    timer))

(defun cancel-timer (timer)
  (trivial-timers:unschedule-timer timer))

(defun send-after (time message &key (actor (current-actor)))
  (call-after time (lambda () (send actor message))))

(defun send-interval (interval message &key (actor (current-actor)) (delay 0))
  (call-interval interval (lambda () (send actor message)) :delay delay))

(defun exit-after (time reason &optional (actor (current-actor)))
  (call-after time (lambda () (exit reason actor))))

(defun kill-after (time &optional (actor (current-actor)))
  (call-after time (lambda () (kill actor))))
