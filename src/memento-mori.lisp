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
   #:trap-exits-p
   #:enable-trap-exits
   #:disable-trap-exits
   #:exit
   #:exit-reason
   #:kill
   #:killed
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
  thread
  driver
  exit-signal
  trap-exits-p)

(defmethod print-object ((actor actor) stream)
  (print-unreadable-object (actor stream :type t :identity t)))

(defun spawn (driver &key scheduler trap-exits-p)
  (when (and (null scheduler)
             (null (current-actor)))
    (error "A scheduler is required when SPAWN is called outside the context of an actor."))
  (make-actor :scheduler (or scheduler (actor-scheduler (current-actor)))
              :driver driver
              :trap-exits-p trap-exits-p))

(defvar *current-actor* nil)
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

(defun trap-exits-p (&aux (actor (current-actor)))
  (assert (not (null actor)))
  (actor-trap-exits-p actor))

(defun enable-trap-exits (&aux (actor (current-actor)))
  (setf (actor-trap-exits-p actor) t)
  (values))

(defun disable-trap-exits (&aux (actor (current-actor)))
  (setf (actor-trap-exits-p actor) nil)
  (values))

(define-condition exit (condition)
  ((reason :initarg :reason :reader exit-reason)))

(defstruct remote-exit
  (from nil :read-only t)
  (reason nil :read-only t))
(defmethod print-object ((remote-exit remote-exit) stream)
  (print-unreadable-object (remote-exit stream :type t)
    (format stream "~S"
            (remote-exit-reason remote-exit))))

(defun exit (reason &optional (actor (current-actor)))
  (assert (or (null actor) (actor-p actor)))
  (cond ((eq actor (current-actor))
         (signal (make-condition 'exit :reason reason)))
        ((actor-trap-exits-p actor)
         (send actor (make-remote-exit :from (current-actor)
                                       :reason reason)))
        ((actor-alive-p actor)
         (let ((exit (make-condition 'exit :reason reason)))
           (when (compare-and-swap (actor-exit-signal actor) nil exit)
             (when-let (thread (actor-thread actor))
               (bt:interrupt-thread thread
                                    (lambda ()
                                      (when (eq thread (actor-thread actor))
                                        (signal exit)))))
             (on-new-actor-message (actor-scheduler actor) actor))))
        (t nil))
  (values))

(defun kill (actor)
  (assert (actor-p actor))
  (when (actor-alive-p actor)
    (let ((exit (make-condition 'exit :reason 'killed)))
      (when (compare-and-swap (actor-exit-signal actor) nil exit)
        (when-let (thread (actor-thread actor))
          (bt:interrupt-thread thread
                               (lambda ()
                                 (when (eq thread (actor-thread actor))
                                   (signal exit)))))
        (on-new-actor-message (actor-scheduler actor) actor))))
  (values))

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
    (without-interrupts
      (tagbody :keep-going
         (let ((actor (dequeue queue)))
           (cond ((and actor (actor-alive-p actor))
                  (setf (actor-thread actor) (bt:current-thread))
                  (if-let (signal (actor-exit-signal actor))
                    (setf (actor-alive-p actor) nil
                          (actor-active-p actor) nil)
                    (multiple-value-bind (val got-val-p)
                        (dequeue (actor-queue actor))
                      (cond (got-val-p
                             (let ((*current-actor* actor))
                               (handler-case
                                   (with-interrupts
                                     (handle-message (actor-driver actor) val)
                                     (enqueue actor queue))
                                 ((or error exit) (e)
                                   (declare (ignore e))
                                   (setf (actor-alive-p actor) nil
                                         (actor-active-p actor) nil))))
                             (notify-actor-waiter scheduler))
                            (t
                             (unless (compare-and-swap (actor-active-p actor) t nil)
                               (enqueue actor queue))))))
                  (setf (actor-thread actor) nil)
                  (values))
                 (t
                  (wait-for-actors scheduler)
                  (go :keep-going))))))))

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

(defun remote-exit-test ()
  (let* ((scheduler (make-threaded-scheduler 2))
         (victim (spawn (lambda (msg)
                          (print "Got a message")
                          (print msg)
                          (sleep 5)
                          (print "Completed!"))
                        :scheduler scheduler))
         (bad-guy (spawn (rcurry 'exit victim) :scheduler scheduler)))
    (send victim 'hi)
    (send bad-guy 'mwahahaaaa)
    (sleep 1)
    (send victim 'rip)
    (print (actor-alive-p victim))
    (sleep 1)
    (stop-threaded-scheduler scheduler)))

(defun trap-exit-test ()
  (let* ((scheduler (make-threaded-scheduler 2))
         (trapping (spawn (lambda (msg)
                            (print msg)
                            (sleep 1)
                            (print "Done"))
                          :scheduler scheduler
                          :trap-exits-p t)))
    (send trapping 'hello)
    (sleep 1)
    (exit 'regular-exit trapping)
    (sleep 1)
    (send trapping 'gonnadiesoon)
    (sleep 1)
    (kill trapping)
    (sleep 1)
    (actor-alive-p trapping)))
