(cl:defpackage #:memento-mori
  (:use #:cl #:alexandria #:memento-mori.utils #:memento-mori.queue)
  (:nicknames #:mori)
  (:export
   ;; Actors
   #:spawn
   #:current-actor
   #:send
   #:on-init
   #:on-message
   #:on-shutdown
   #:actor-alive-p
   #:call-with-next-message
   #:with-next-message
   #:reject-message

   ;; Monitors
   #:monitor
   #:demonitor
   #:monitorp
   #:monitor-exit
   #:monitor-exit-p
   #:monitor-exit-monitor
   #:monitor-exit-from
   #:monitor-exit-reason
   #:actor-down

   ;; Links
   #:link
   #:unlink

   ;; Exits
   #:trap-exits-p
   #:enable-trap-exits
   #:disable-trap-exits
   #:remote-exit
   #:remote-exit-p
   #:remote-exit-reason
   #:remote-exit-from
   #:exit
   #:exit-reason
   #:kill
   #:killed

   ;; Schedulers
   #:make-threaded-scheduler
   #:stop-threaded-scheduler))
(cl:in-package #:memento-mori)

#+nil
(declaim (optimize speed))
;;;
;;; Actors
;;;
(defstruct actor
  scheduler
  (signals (make-queue))
  (queue (make-queue))
  (save-queue (make-queue))
  (message-handler 'init-handler)
  save-queue-checked-p
  (alive-p t)
  active-p
  thread
  driver
  links
  monitors
  (monitor-lock (bt:make-lock))
  trap-exits-p
  name
  named-p
  debug-p)

(defmethod print-object ((actor actor) stream)
  (print-unreadable-object (actor stream :type t :identity t)
    (maybe-format-actor-name actor stream)
    (format stream "[~a msgs]" (queue-count (actor-queue actor)))))

(defun %current-actor-debug-p ()
  (when-let (actor (current-actor))
    (actor-debug-p actor)))

(defvar +init-message+ (gensym "SERVER-INIT-"))

(defun spawn (driver &key
                       scheduler
                       trap-exits-p
                       linkp
                       monitorp
                       (name nil namep)
                       (debugp (%current-actor-debug-p)))
  (when (and (null scheduler)
             (null (current-actor)))
    (error "A scheduler is required when SPAWN is called outside the context of an actor."))
  (let ((actor (make-actor :scheduler (or scheduler (actor-scheduler (current-actor)))
                           :driver driver
                           :trap-exits-p trap-exits-p
                           :debug-p debugp)))
    (when namep (register name actor))
    (when linkp (link actor))
    (send actor +init-message+)
    (if monitorp
        (values actor (monitor actor))
        actor)))

(defun init-handler (msg)
  (if (eq msg +init-message+)
      (on-init (actor-driver (current-actor)))
      (reject-message)))

(defun msg-handler (msg)
  (on-message (actor-driver (current-actor)) msg))

(defvar +new-handler+ (gensym "NEW-HANDLER-"))

(defun call-with-next-message (test &aux (actor (current-actor)))
  (assert (not (null actor)))
  (throw +new-handler+ test))

(defmacro with-next-message ((msg-var) &body body)
  `(call-with-next-message (lambda (,msg-var) ,@body)))

(defvar +reject-message+ (gensym "REJECT-MESSAGE-"))

(defun reject-message ()
  (assert (not (null (current-actor))))
  (throw +reject-message+ t))

(defvar *current-actor* nil)
(defun current-actor ()
  *current-actor*)

(defun send (actor message &aux (actor (ensure-actor actor)))
  (when (actor-alive-p actor)
    (enqueue message (actor-queue actor))
    (on-new-actor-message (actor-scheduler actor) actor))
  message)

(defgeneric on-new-actor-message (scheduler actor))

;;;
;;; Registration
;;;
(defvar *registered-actors* (make-hash-table :test #'eq))
(defvar *registration-lock* (bt:make-lock))

(define-condition no-such-actor (error)
  ((name :initarg :name :reader no-such-actor-name))
  (:report (lambda (e stream)
             (format stream "~S is not the name of a registered actor."
                     (no-such-actor-name e)))))

(define-condition actor-already-exists (error)
  ((name :initarg :name :reader actor-already-exists-name)
   (existing-actor :initarg :existing :reader actor-already-exists-existing-actor))
  (:report (lambda (e stream)
             (format stream "~S is already registered as ~s."
                     (actor-already-exists-existing-actor e)
                     (actor-already-exists-name e)))))

(defun list-registered-names ()
  "Returns a list of all registered actor names."
  (bt:with-recursive-lock-held (*registration-lock*)
    (hash-table-keys *registered-actors*)))

(defun find-actor (name &optional (errorp t))
  "Returns an `actor` named by `name`. If `errorp` is true, signals a
condition of type `no-such-actor` when an actor is not found under that
name. If `errorp` is false, returns nil when no actor is found."
  (check-type name symbol "a valid actor name")
  (bt:with-recursive-lock-held (*registration-lock*)
    (multiple-value-bind (actor foundp)
        (gethash name *registered-actors*)
      (cond (foundp actor)
            (errorp (error 'no-such-actor :name name))
            (t nil)))))

(defun register (name actor &optional (errorp t))
  "Registers `actor` under `name`. If `errorp` is true, signals a condition of
type `actor-already-exists` if there is already an actor registered under
that name. Otherwise, if `errorp` is false, the existing registration is
replaced."
  (check-type name symbol "a valid actor name")
  (check-type actor actor "an actor object")
  (bt:with-recursive-lock-held (*registration-lock*)
    (when errorp
      (when-let ((old-actor (find-actor name nil)))
        (restart-case
            (error 'actor-already-exists
                   :name name
                   :existing old-actor)
          (replace ()
            :report "Replace the registration. Existing actor will continue to run."
            nil)
          (shutdown-and-replace ()
            :report "Replace the registration. Existing actor will be sent a shutdown request."
            (exit 'shutdown old-actor))
          (kill-and-replace ()
            :report "Replace the registration. Existing actor will be killed."
            (kill old-actor)))))
    (setf (actor-name actor) name
          (actor-named-p actor) t
          (gethash name *registered-actors*) actor)))

(defun ensure-actor (actor)
  (etypecase actor
    (actor actor)
    (symbol (find-actor actor))))

(defun unregister (name &optional (errorp t))
  "Removes the actor registration associated with `name`. If `errorp` is true,
signals a condition of type `no-such-actor` if there is no actor registered
under that name. If false, returns nil."
  (check-type name symbol "a valid actor name")
  (bt:with-recursive-lock-held (*registration-lock*)
    (when (and (null (remhash name *registered-actors*))
               errorp)
      (error 'no-such-actor :name name))))

(defun maybe-format-actor-name (actor stream)
  (bt:with-recursive-lock-held (*registration-lock*)
    (when (actor-named-p actor)
      (format stream "~s " (actor-name actor)))))

;;;
;;; Links
;;;

;; NOTE - yes, this is a cop-out. :(
(defvar *link-lock* (bt:make-lock))

(defun link (actor &aux (self (current-actor)) (actor (ensure-actor actor)))
  (bt:with-lock-held (*link-lock*)
    (cond ((actor-alive-p actor)
           (pushnew actor (actor-links self) :test 'eq)
           (pushnew self (actor-links actor) :test 'eq))
          (t
           (exit 'actor-dead self))))
  (values))

(defun unlink (actor &aux (self (current-actor)) (actor (ensure-actor actor)))
  (bt:with-lock-held (*link-lock*)
    (removef (actor-links actor) self :test 'eq)
    (removef (actor-links self) actor :test 'eq))
  (values))

(defun notify-links (actor exit)
  (bt:with-lock-held (*link-lock*)
    (when-let (links (actor-links actor))
      (map nil (lambda (linked)
                 (exit exit linked)
                 (deletef (actor-links linked) actor :test #'eq))
           links))))

;;;
;;; Monitors
;;;
(defmacro with-monitor-lock ((actor) &body body)
  `(bt:with-lock-held ((actor-monitor-lock ,actor))
     ,@body))

(defstruct (monitor (:predicate monitorp))
  (observer nil :read-only t)
  (monitored-actor nil :read-only t))

(defmethod print-object ((monitor monitor) stream)
  (print-unreadable-object (monitor stream :type t :identity t)
    (format stream "Actor: ~a" (monitor-monitored-actor monitor))))

(defun monitor (actor &aux (observer (current-actor)) (actor (ensure-actor actor)))
  (let ((monitor (make-monitor :observer observer :monitored-actor actor)))
    (with-monitor-lock (actor)
      (if (actor-alive-p actor)
          (push monitor (actor-monitors actor))
          (send observer (make-monitor-exit
                          :monitor monitor
                          :from actor
                          :reason 'actor-down))))
    monitor))

(defun demonitor (monitor)
  (let ((actor (monitor-monitored-actor monitor)))
    (with-monitor-lock (actor)
      (removef (actor-monitors actor) monitor)))
  (values))

(defstruct monitor-exit
  (monitor nil :read-only t)
  (from nil :read-only t)
  (reason nil :read-only t))
(defmethod print-object ((monitor-exit monitor-exit) stream)
  (print-unreadable-object (monitor-exit stream :type t)
    (format stream "~S"
            (monitor-exit-reason monitor-exit))))

(defun notify-monitors (actor exit)
  (with-monitor-lock (actor)
    (map nil (lambda (monitor)
               (send (monitor-observer monitor)
                     (make-monitor-exit :monitor monitor
                                        :from actor
                                        :reason (exit-reason exit))))
         (actor-monitors actor))
    (setf (actor-monitors actor) nil)))

;;;
;;; Exits
;;;
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
(define-condition %killed (condition) ())

(defstruct remote-exit
  (from nil :read-only t)
  (reason nil :read-only t))
(defmethod print-object ((remote-exit remote-exit) stream)
  (print-unreadable-object (remote-exit stream :type t)
    (format stream "~@[[from ~A] ~]~S"
            (remote-exit-from remote-exit)
            (remote-exit-reason remote-exit))))

(defvar +kill-signal+ (gensym "KILL-SIGNAL-"))

(defun exit (reason
             &optional (actor nil actorp)
             &aux (actor (when actorp (ensure-actor actor))))
  (cond ((not actorp)
         (signal (if (eq reason +kill-signal+)
                     (make-condition '%killed)
                     (make-condition 'exit :reason reason))))
        ((actor-alive-p actor)
         (enqueue (make-remote-exit :from (current-actor) :reason reason)
                  (actor-signals actor))
         (when-let (thread (actor-thread actor))
           (bt:interrupt-thread thread
                                (lambda ()
                                  (when (eq thread (actor-thread actor))
                                    (process-signals actor)))))
         (on-new-actor-message (actor-scheduler actor) actor))
        (t nil))
  (values))

(defun kill (actor)
  (exit +kill-signal+ actor))

;;;
;;; Scheduler
;;;
(defgeneric event-step (scheduler))

(defstruct (threaded-scheduler (:constructor %make-threaded-scheduler))
  (active-actors (make-queue))
  (activity-lock (bt:make-lock))
  (activity-condvar (bt:make-condition-variable))
  (idle-thread-count 0)
  threads)

(defmethod print-object ((scheduler threaded-scheduler) stream)
  (print-unreadable-object (scheduler stream :type t :identity t)
    (format stream "~a threads (~a idle)"
            (length (threaded-scheduler-threads scheduler))
            (threaded-scheduler-idle-thread-count scheduler))))

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

(defvar +unhandled-exit+ (gensym "UNHANDLED-EXIT-"))
(defvar *debugger-lock* (bt:make-lock))

(defgeneric on-init (driver)
  (:method ((driver t)) t))

(defgeneric on-message (driver message)
  (:method ((driver t) (message t))
    (error "No ON-MESSAGE method defined for ~s with message ~s."
           driver message))
  (:method ((driver function) message)
    (funcall driver message))
  (:method ((driver symbol) message)
    (funcall driver message)))

(defgeneric on-shutdown (driver reason)
  (:method ((driver t) (reason t)) t))

(defmethod event-step ((scheduler threaded-scheduler))
  (let ((queue (threaded-scheduler-active-actors scheduler)))
    (without-interrupts
      (tagbody :keep-going
         (let ((actor (dequeue queue)))
           (cond ((and actor (actor-alive-p actor))
                  (setf (actor-thread actor) (bt:current-thread))
                  (catch +unhandled-exit+
                    (let ((*current-actor* actor))
                      (process-signals actor)
                      (handler-bind
                          ((error (lambda (e)
                                    (when (actor-debug-p actor)
                                      (bt:with-recursive-lock-held (*debugger-lock*)
                                        (invoke-debugger e)))
                                    (actor-death
                                     actor
                                     (make-condition 'exit :reason e))
                                    (throw +unhandled-exit+ nil)))
                           (exit (lambda (e)
                                   (actor-death actor e)
                                   (throw +unhandled-exit+ nil))))
                        (restart-case
                            (loop for (val got-val-p) = (multiple-value-list (get-message actor))
                               while got-val-p
                               if (let (message-accepted-p)
                                    (setf (actor-message-handler actor)
                                          (or (catch +new-handler+
                                                (catch +reject-message+
                                                  (with-interrupts
                                                    (funcall
                                                     (actor-message-handler actor)
                                                     val))
                                                  (setf message-accepted-p t))
                                                (unless message-accepted-p
                                                  (throw +new-handler+
                                                    (actor-message-handler actor)))
                                                nil)
                                              'msg-handler))
                                    message-accepted-p)
                               do
                                 (enqueue actor queue)
                                 (setf (actor-save-queue-checked-p actor) nil)
                                 (notify-actor-waiter scheduler)
                                 (return)
                               finally
                                 (unless (compare-and-swap (actor-active-p actor) t nil)
                                   (enqueue actor queue)))
                          (abort ()
                            :report "Kill the current actor."
                            (kill actor))))))
                  (setf (actor-thread actor) nil)
                  (values))
                 (t
                  (wait-for-actors scheduler)
                  (go :keep-going))))))))

(defun get-message (actor)
  (multiple-value-bind (msg got-val-p)
      (unless (actor-save-queue-checked-p actor)
        (dequeue (actor-save-queue actor)))
    (cond (got-val-p
           (values msg got-val-p))
          (t
           (setf (actor-save-queue-checked-p actor) t)
           (dequeue (actor-queue actor))))))

(defun process-signals (actor)
  (loop for signal = (dequeue (actor-signals actor))
     while signal
     do (let ((reason (remote-exit-reason signal)))
          (cond ((and (actor-trap-exits-p actor)
                      (not (eq reason +kill-signal+)))
                 (enqueue signal (actor-queue actor)))
                (t
                 (actor-death actor
                              (make-condition
                               'exit
                               :reason (if (eq reason +kill-signal+)
                                           'killed
                                           reason)))
                 (throw +unhandled-exit+ nil))))
     finally (return t)))

(defun wait-for-actors (scheduler)
  (bt:with-lock-held ((threaded-scheduler-activity-lock scheduler))
    (atomic-incf (threaded-scheduler-idle-thread-count scheduler))
    (bt:condition-wait (threaded-scheduler-activity-condvar scheduler)
                       (threaded-scheduler-activity-lock scheduler))
    (atomic-decf (threaded-scheduler-idle-thread-count scheduler))))

(defun notify-actor-waiter (scheduler)
  (when (plusp (threaded-scheduler-idle-thread-count scheduler))
    (bt:with-lock-held ((threaded-scheduler-activity-lock scheduler))
      (bt:condition-notify (threaded-scheduler-activity-condvar scheduler)))))

(defun actor-death (actor exit &aux (*current-actor* actor))
  (ignore-some-conditions (error exit) (on-shutdown (actor-driver actor) (exit-reason exit)))
  (setf (actor-alive-p actor) nil
        (actor-active-p actor) nil)
  (notify-links actor exit)
  (notify-monitors actor exit))
