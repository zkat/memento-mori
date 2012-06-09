(cl:defpackage #:memento-mori.utils
  (:use #:cl #:alexandria)
  (:export
   ;; Queues
   #:make-queue
   #:enqueue
   #:dequeue
   #:queue-empty-p
   #:dequeue-all
   ;; Misc utils
   #:without-interrupts
   #:with-interrupts
   #:timeout
   #:with-timeout))
(cl:in-package #:memento-mori.utils)

(defun make-queue () (cons nil nil))

(defun enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q)
  (pop (car q)))

(defun queue-empty-p (q)
  (null (car q)))

(defun dequeue-all (q)
  (prog1 (car q)
    (setf (car q) nil
          (cdr q) nil)))

(defmacro without-interrupts (&body body)
  #+sbcl
  `(sb-sys:without-interrupts ,@body)
  #+ccl
  `(ccl:without-interrupts ,@body)
  #-(or sbcl ccl)
  (error "Unsupported"))

(defmacro with-interrupts (&body body)
  #+sbcl
  `(sb-sys:with-interrupts ,@body)
  #+ccl
  `(ccl:with-interrupts-enabled ,@body)
  #-(or sbcl ccl)
  (error "Unsupported"))

(defmacro with-timeout (expires &body body)
  `(flet ((timeout-body () ,@body))
     (let ((expires ,expires))
       (block over-here
         (let ((timer (trivial-timers:make-timer
                       (lambda ()
                         (return-from over-here (values nil nil))))))
           (trivial-timers:schedule-timer timer expires)
           (unwind-protect (values (timeout-body) t)
             (trivial-timers:unschedule-timer timer)))))))
