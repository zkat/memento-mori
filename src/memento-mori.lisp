(cl:defpackage #:memento-mori
  (:use #:cl #:alexandria #:memento-mori.utils #:memento-mori.queue)
  (:nicknames #:mori)
  (:export
   ;; Actors
   #:spawn
   #:current-actor
   #:send
   #:handle-message
   ;; #:ensure-persistent-binding
   ;; #:remove-persistent-binding
   ;; Links
   #:link
   #:unlink
   ;; Exits
   #:trap-exits-p
   #:enable-trap-exits
   #:disable-trap-exits
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
  (alive-p t)
  active-p
  thread
  driver
  links
  trap-exits-p
  bindings
  debug-p)

(defmethod print-object ((actor actor) stream)
  (print-unreadable-object (actor stream :type t :identity t)))

(defun %current-actor-debug-p ()
  (when-let (actor (current-actor))
    (actor-debug-p actor)))

(defun spawn (driver &key
                       scheduler
                       trap-exits-p
                       linkp
                       initial-bindings
                       (debugp (%current-actor-debug-p)))
  (when (and (null scheduler)
             (null (current-actor)))
    (error "A scheduler is required when SPAWN is called outside the context of an actor."))
  (let ((actor (make-actor :scheduler (or scheduler (actor-scheduler (current-actor)))
                           :driver driver
                           :trap-exits-p trap-exits-p
                           :bindings (loop for (binding . value) in initial-bindings
                                        collect (cons binding value))
                           :debug-p debugp)))
    (when linkp
      (link actor))
    actor))

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

#+nil
(defun ensure-persistent-binding (symbol)
  (pushnew (cons symbol (symbol-value symbol))
           (actor-bindings (current-actor))
           :test #'equal)
  (values))

#+nil
(defun remove-persistent-binding (symbol)
  (deletef (actor-bindings (current-actor))
           symbol :key #'car)
  (values))

;;;
;;; Links
;;;

;; NOTE - yes, this is a cop-out. :(
(defvar *link-lock* (bt:make-lock))

(defun link (actor &aux (self (current-actor)))
  (bt:with-lock-held (*link-lock*)
    (cond ((actor-alive-p actor)
           (pushnew actor (actor-links self) :test 'eq)
           (pushnew self (actor-links actor) :test 'eq))
          (t
           (exit 'actor-dead))))
  (values))

(defun unlink (actor &aux (self (current-actor)))
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
    (format stream "~S [from ~A]"
            (remote-exit-reason remote-exit)
            (remote-exit-from remote-exit))))

(defvar +kill-signal+ (gensym "KILL-SIGNAL-"))

(defun exit (reason &optional (actor (current-actor)))
  (assert (or (null actor) (actor-p actor)))
  (cond ((eq actor (current-actor))
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
  (assert (actor-p actor))
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

(defmethod event-step ((scheduler threaded-scheduler))
  (let ((queue (threaded-scheduler-active-actors scheduler)))
    (without-interrupts
      (tagbody :keep-going
         (let ((actor (dequeue queue)))
           (cond ((and actor (actor-alive-p actor))
                  (setf (actor-thread actor) (bt:current-thread))
                  (catch +unhandled-exit+
                    (let ((*current-actor* actor))
                      (process-signals actor))
                    (multiple-value-bind (val got-val-p)
                        (dequeue (actor-queue actor))
                      (cond (got-val-p
                             (let ((*current-actor* actor))
                               (progv
                                   (mapcar #'car (actor-bindings actor))
                                   (mapcar #'cdr (actor-bindings actor))
                                 (unwind-protect
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
                                            (with-interrupts
                                              (handle-message (actor-driver actor) val)
                                              (enqueue actor queue))
                                          (abort ()
                                            :report "Kill the current actor."
                                            (kill actor))))
                                   (setf (actor-bindings actor)
                                         (loop for (binding . nil) in (actor-bindings actor)
                                            when (boundp binding)
                                            collect (cons binding (symbol-value binding)))))))
                             (notify-actor-waiter scheduler))
                            (t
                             (unless (compare-and-swap (actor-active-p actor) t nil)
                               (enqueue actor queue))))))
                  (setf (actor-thread actor) nil)
                  (values))
                 (t
                  (wait-for-actors scheduler)
                  (go :keep-going))))))))

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

(declaim (inline wait-for-actors))
(defun wait-for-actors (scheduler)
  (bt:with-lock-held ((threaded-scheduler-activity-lock scheduler))
    (atomic-incf (threaded-scheduler-idle-thread-count scheduler))
    (bt:condition-wait (threaded-scheduler-activity-condvar scheduler)
                       (threaded-scheduler-activity-lock scheduler))
    (atomic-decf (threaded-scheduler-idle-thread-count scheduler))))

(declaim (inline notify-actor-waiter))
(defun notify-actor-waiter (scheduler)
  (when (plusp (threaded-scheduler-idle-thread-count scheduler))
    (bt:with-lock-held ((threaded-scheduler-activity-lock scheduler))
      (bt:condition-notify (threaded-scheduler-activity-condvar scheduler)))))

(declaim (inline actor-death))
(defun actor-death (actor exit)
  (setf (actor-alive-p actor) nil
        (actor-active-p actor) nil)
  (notify-links actor exit))

;;;
;;; Testing
;;;
(defun speed-test (scheduler
                   &key
                     (message-count 100000)
                     (actor-count 10))
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
       do (send actor message)))
  scheduler)

(defun local-exit-test (scheduler)
  (let ((actor (spawn (lambda (msg)
                        (print "Got a message")
                        (exit msg)
                        (print "After exit"))
                      :scheduler scheduler)))
    (send actor 'fail)
    (send actor 'again)
    (sleep 1)
    actor)
  scheduler)

(defun remote-exit-test (scheduler)
  (let* ((victim (spawn (lambda (msg)
                          (print "Got a message")
                          (print msg)
                          (sleep 5)
                          (print "Completed!"))
                        :scheduler scheduler))
         (bad-guy (spawn (lambda (msg)
                           (print "I got scheduled, too!")
                           (exit msg victim)
                           (print "message sent"))
                         :scheduler scheduler)))
    (send victim 'hi)
    (send bad-guy 'mwahahaaaa)
    (sleep 1)
    (send victim 'rip)
    (sleep 1)
    (print (actor-alive-p victim))
    (print (actor-alive-p bad-guy)))
  scheduler)

(defun trap-exits-test (scheduler)
  (let ((trapping (spawn (lambda (msg)
                           (print msg)
                           (sleep 1)
                           (print "Done"))
                         :scheduler scheduler
                         :trap-exits-p t)))
    (send trapping 'hello)
    (sleep 1)
    (send (spawn (rcurry #'exit trapping) :scheduler scheduler) 'regular-exit)
    (sleep 1)
    (send trapping 'gonnadiesoon)
    (sleep 1)
    (kill trapping)
    (sleep 1)
    (actor-alive-p trapping)
    scheduler))

(defun links-test (n scheduler)
  (labels ((chain (n)
             (cond ((= n 0)
                    (error "I can't take this anymore!"))
                   (t
                    (send (spawn #'chain :linkp t) (1- n))))))
    (send
     (spawn (let ((start-time (get-internal-real-time)))
              (lambda (msg)
                (if (integerp msg)
                    (chain msg)
                    (let ((total (/ (- (get-internal-real-time) start-time)
                                    internal-time-units-per-second)))
                      (format t "~&Chain done. ~a actors in ~f seconds (~f/s).~%"
                              n total (/ n total))))))
            :trap-exits-p t
            :scheduler scheduler)
     n)
    scheduler))

(defun dynamic-bindings-test (scheduler)
  (let ((actor (spawn (lambda (increment)
                        ;; (print (boundp (symbol-value '*evenp*)))
                        ;; (if (evenp (symbol-value '*test*))
                        ;;     (ensure-persistent-binding '*evenp*)
                        ;;     (remove-persistent-binding '*evenp*))
                        (when (> 10 (print (incf (symbol-value '*test*) increment)))
                          (send (current-actor) increment)))
                      :initial-bindings '((*test* . 1))
                      :scheduler scheduler)))
    (send actor 1)))

(defun debugger-test (scheduler)
  (let ((actor (spawn (lambda (msg)
                        (send (spawn (curry #'error "Dying from ~a."))
                              msg))
                      :scheduler scheduler
                      :debugp t
                      :trap-exits-p t)))
    (loop repeat 5 do (send actor 'fail))))
