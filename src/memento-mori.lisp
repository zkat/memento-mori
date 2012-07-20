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
  (loop
     for actor = (dequeue *active-actors*)
     if actor do
       (multiple-value-bind (val got-val-p)
           (dequeue (actor-queue actor))
         (cond (got-val-p
                (let ((*current-actor* actor))
                  (funcall (actor-on-receive actor) val))
                (enqueue actor *active-actors*)
                (notify-actor-waiter)
                (loop-finish))
               (t
                (unless (compare-and-swap (actor-active-p actor) t nil)
                  (enqueue actor *active-actors*))
                (loop-finish))))
     else do
       (if blockp
           (wait-for-actors)
           (loop-finish)))
  (values))

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
