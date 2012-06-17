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
  "Calls `function` after `time` seconds have elapsed. `call-after` will
interrupt the current thread in order to execute `function`'s code."
  (let ((timer (trivial-timers:make-timer function)))
    (trivial-timers:schedule-timer timer time)
    timer))

(defun call-interval (interval function &key (delay 0))
  "Calls `function` every `interval` seconds until the returned timer is
cancelled. `delay` is the number of seconds to wait before the first
call."
  (let ((timer (trivial-timers:make-timer function)))
    (trivial-timers:schedule-timer timer delay :repeat-interval interval)
    timer))

(defun cancel-timer (timer)
  "Given a `timer` returned by one of the `mori-timer` functions, unschedules
that timer and prevents it from executing in the future."
  (trivial-timers:unschedule-timer timer))

(defun send-after (time message &key (actor (current-actor)))
  "Sends `message` to `actor` after `time` seconds have elapsed."
  (call-after time (lambda () (send actor message))))

(defun send-interval (interval message &key (actor (current-actor)) (delay 0))
  "Sends `message` to `actor` every `interval` seconds until the returned
timer is cancelled. `delay` is the number of seconds to wait before the
first message is sent."
  (call-interval interval (lambda () (send actor message)) :delay delay))

(defun exit-after (time reason &optional (actor (current-actor)))
  "Calls `mori:exit` on `actor` with reason `reason` after `time` seconds have
elapsed."
  (call-after time (lambda () (exit reason actor))))

(defun kill-after (time &optional (actor (current-actor)))
  "Kills `actor` after `time` seconds have elapsed."
  (call-after time (lambda () (kill actor))))
