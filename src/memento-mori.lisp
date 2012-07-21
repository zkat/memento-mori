(cl:defpackage #:memento-mori
  (:use #:cl #:alexandria #:memento-mori.utils #:memento-mori.queue)
  (:nicknames #:mori)
  (:export #:send
           #:spawn
           #:current-actor
           #:event-loop
           #:event-step
           #:start-event-thread))
(cl:in-package #:memento-mori)

;;;
;;; Actors
;;;
(defstruct actor
  (queue (make-queue))
  active-p
  on-receive)

(defmethod print-object ((actor actor) stream)
  (print-unreadable-object (actor stream :type t :identity t)))

(defvar *active-actors* (make-queue))
(defvar *actor-activity-lock* (bt:make-lock))
(defvar *actor-activity-condvar* (bt:make-condition-variable))
(defvar *idle-thread-p* (vector nil))

(defvar *current-actor*)
(defun current-actor ()
  *current-actor*)

(defun send (actor message)
  (enqueue message (actor-queue actor))
  (notify-actor-waiter)
  (when (compare-and-swap (actor-active-p actor) nil t)
    (enqueue actor *active-actors*))
  message)

(defun spawn (on-receive)
  (make-actor :on-receive on-receive))

;;;
;;; Event loop
;;;
(defun event-loop ()
  (loop (event-step)))

(defun event-step (&key (blockp t))
  (tagbody :keep-going
     (let ((actor (dequeue *active-actors*)))
       (cond (actor
              (multiple-value-bind (val got-val-p)
                  (dequeue (actor-queue actor))
                (cond (got-val-p
                       (let ((*current-actor* actor))
                         (funcall (actor-on-receive actor) val))
                       (enqueue actor *active-actors*)
                       (notify-actor-waiter))
                      (t
                       (unless (compare-and-swap (actor-active-p actor) t nil)
                         (enqueue actor *active-actors*)))))
              (values))
             (blockp
              (wait-for-actors)
              (go :keep-going))
             (t
              (values))))))

;;;
;;; Event threads
;;;
(defun start-event-thread ()
  (bt:make-thread 'event-loop))

;;;
;;; Utils
;;;
(declaim (inline wait-for-actors))
(defun wait-for-actors ()
  (bt:with-lock-held (*actor-activity-lock*)
    (setf (svref *idle-thread-p* 0) t)
    (bt:condition-wait *actor-activity-condvar* *actor-activity-lock*)))

(declaim (inline notify-actor-waiter))
(defun notify-actor-waiter ()
  (when (compare-and-swap (svref *idle-thread-p* 0) t nil)
    (bt:with-lock-held (*actor-activity-lock*)
      (bt:condition-notify *actor-activity-condvar*))))

;;;
;;; Testing
;;;
(defun test (&optional (message-count 1000000))
  (let ((test-actors
         (loop repeat 10 collect
              (spawn
               (lambda (x &aux (counter (car x)) (start-time (cdr x)))
                 (if (> counter 0)
                     (send (current-actor) (cons (1- counter) start-time))
                     (print `(stop time ,(/ (- (get-internal-real-time) start-time)
                                            internal-time-units-per-second 1.0)))))))))
    (loop for actor in test-actors
         do (send actor (cons message-count (get-internal-real-time))))))
