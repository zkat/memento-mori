(cl:defpackage #:memento-mori
  (:use #:cl #:alexandria #:memento-mori.utils #:memento-mori.queue)
  (:nicknames #:mori)
  (:export
   ;; Actors
   #:spawn
   #:current-actor
   #:send
   #:handle-message
   #:actor-scheduler
   #:exit
   #:exit-reason
   ;; Schedulers
   #:make-threaded-scheduler
   #:stop-threaded-scheduler))
(cl:in-package #:memento-mori)

;;;
;;; Actors
;;;
(defstruct actor
  scheduler
  (queue (make-queue))
  (alive-p t)
  active-p
  driver)

(defmethod print-object ((actor actor) stream)
  (print-unreadable-object (actor stream :type t :identity t)))

(defun spawn (driver &key (scheduler (actor-scheduler (current-actor))))
  (make-actor :scheduler scheduler :driver driver))

(defvar *current-actor*)
(defun current-actor ()
  *current-actor*)

(defun send (actor message)
  (when (actor-alive-p actor)
    (enqueue message (actor-queue actor))
    (on-new-actor-message (actor-scheduler actor) actor))
  message)

(defgeneric on-new-actor-message (scheduler actor))

(defgeneric handle-message (driver message)
  (:method ((driver function) message)
    (funcall driver message))
  (:method ((driver symbol) message)
    (funcall driver message)))

(define-condition exit (condition)
  ((reason :initarg :reason :reader exit-reason)))

(defun exit (reason)
  (signal (make-condition 'exit :reason reason)))

;;;
;;; Scheduler
;;;
(defgeneric event-step (scheduler))

(defstruct (threaded-scheduler (:constructor %make-threaded-scheduler))
  (active-actors (make-queue))
  (activity-lock (bt:make-lock))
  (activity-condvar (bt:make-condition-variable))
  idle-thread-p
  threads)

(defmethod print-object ((scheduler threaded-scheduler) stream)
  (print-unreadable-object (scheduler stream :type t :identity t)
    (format stream "[~a threads]" (length (threaded-scheduler-threads scheduler)))))

(defun make-threaded-scheduler (thread-count)
  (let* ((scheduler (%make-threaded-scheduler))
         (threads (loop repeat thread-count collect
                       (bt:make-thread (curry 'event-loop scheduler)))))
    (setf (threaded-scheduler-threads scheduler) threads)
    scheduler))

(defun stop-threaded-scheduler (scheduler)
  (map nil #'bt:destroy-thread (threaded-scheduler-threads scheduler))
  (setf (threaded-scheduler-threads scheduler) nil))

(defmethod on-new-actor-message ((scheduler threaded-scheduler) actor)
  (notify-actor-waiter scheduler)
  (when (compare-and-swap (actor-active-p actor) nil t)
    (enqueue actor (threaded-scheduler-active-actors scheduler))))

(defun event-loop (scheduler)
  (loop (event-step scheduler)))

(defmethod event-step ((scheduler threaded-scheduler))
  (let ((queue (threaded-scheduler-active-actors scheduler)))
    (tagbody :keep-going
       (let ((actor (dequeue queue)))
         (cond ((and actor (actor-alive-p actor))
                (multiple-value-bind (val got-val-p)
                    (dequeue (actor-queue actor))
                  (cond (got-val-p
                         (when (%handle-message actor val)
                           (enqueue actor queue))
                         (notify-actor-waiter scheduler))
                        (t
                         (unless (compare-and-swap (actor-active-p actor) t nil)
                           (enqueue actor queue)))))
                (values))
               (t
                (wait-for-actors scheduler)
                (go :keep-going)))))))

(defun %handle-message (actor msg)
  (let ((*current-actor* actor))
    (handler-case
        (prog1 t
          (handle-message (actor-driver actor) msg))
      ((or error exit) (e)
        (declare (ignore e))
        (setf (actor-alive-p actor) nil
              (actor-active-p actor) nil)
        nil))))

(declaim (inline wait-for-actors))
(defun wait-for-actors (scheduler)
  (bt:with-lock-held ((threaded-scheduler-activity-lock scheduler))
    (setf (threaded-scheduler-idle-thread-p scheduler) t)
    (bt:condition-wait (threaded-scheduler-activity-condvar scheduler)
                       (threaded-scheduler-activity-lock scheduler))))

(declaim (inline notify-actor-waiter))
(defun notify-actor-waiter (scheduler)
  (when (compare-and-swap (threaded-scheduler-idle-thread-p scheduler)
                          t nil)
    (bt:with-lock-held ((threaded-scheduler-activity-lock scheduler))
      (bt:condition-notify (threaded-scheduler-activity-condvar scheduler)))))

;;;
;;; Testing
;;;
(defun speed-test (&key (message-count 100000) (actor-count 10) (thread-count 6))
  (let ((scheduler (make-threaded-scheduler thread-count)))
    (flet ((handler (x)
             (let ((counter (car x))
                   (start-time (cdr x)))
               (if (> counter 0)
                   (send (current-actor) (cons (1- counter) start-time))
                   (print `(stop time ,(/ (- (get-internal-real-time) start-time)
                                          internal-time-units-per-second 1.0)))))))
      (loop
         repeat actor-count
         for actor = (spawn #'handler :scheduler scheduler)
         for message = (cons message-count (get-internal-real-time))
         do (send actor message)))))

(defun local-exit-test ()
  (let* ((scheduler (make-threaded-scheduler 2))
         (actor (spawn (lambda (msg)
                         (print "Got a message")
                         (exit msg)
                         (print "After exit"))
                       :scheduler scheduler)))
    (send actor 'fail)
    (send actor 'again)
    (sleep 1)
    (stop-threaded-scheduler scheduler)
    actor))
