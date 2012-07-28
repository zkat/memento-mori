(cl:defpackage #:memento-mori.timer
  (:use #:cl #:memento-mori)
  (:nicknames #:mori-timer)
  (:export
   #:cancel-timer
   #:send-after
   #:send-interval))
(cl:in-package #:memento-mori.timer)

;;;
;;; API
;;;
(defun send-after (time message &key (actor (current-actor)))
  "Sends `message` to `actor` after `time` seconds have elapsed."
  (%call-after time (lambda () (send actor message))))

(defun send-interval (interval message &key (actor (current-actor)) (delay 0))
  "Sends `message` to `actor` every `interval` seconds until the returned
timer is cancelled. `delay` is the number of seconds to wait before the
first message is sent."
  (%call-interval interval (lambda () (send actor message)) :delay delay))

(defun cancel-timer (timer)
  "Given a `timer` returned by one of the `mori-timer` functions, unschedules
that timer and prevents it from executing in the future."
  (trivial-timers:unschedule-timer timer))

;;;
;;; Internal
;;;

;; Note: These are internal because interrupting a thread in order to
;; execute something is really not very kosher in mori. Instead, we limit
;; timers to simply sending messages, which we can safely send from any
;; thread we happen to interrupt.
;;
;; There's probably a way to make something like this public without
;; replacing the actors' message queue with a lock-based priority queue,
;; but we won't deal with that just yet
(defun %call-after (time function)
  (let ((timer (trivial-timers:make-timer function)))
    (trivial-timers:schedule-timer timer time)
    timer))

(defun %call-interval (interval function &key (delay 0))
  (let ((timer (trivial-timers:make-timer function)))
    (trivial-timers:schedule-timer timer delay :repeat-interval interval)
    timer))
