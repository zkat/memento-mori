(cl:defpackage #:memento-mori.utils
  (:use #:cl #:alexandria)
  (:export
   #:compare-and-swap
   #:atomic-setf
   #:atomic-incf
   #:atomic-decf
   #:atomic-push
   #:atomic-pop
   #:without-interrupts
   #:with-interrupts
   #:make-light-queue
   #:lq-enq
   #:lq-deq))
(cl:in-package #:memento-mori.utils)

(defmacro compare-and-swap (place old-value new-value)
  #+sbcl
  (let ((old-val-var (gensym "OLD-VALUE-")))
    ` (let ((,old-val-var ,old-value))
        (eq ,old-val-var (sb-ext:compare-and-swap ,place ,old-val-var ,new-value))))
  #+ccl
  `(ccl::conditional-store ,place ,old-value ,new-value)
  #+lispworks
  `(system:compare-and-swap ,place ,old-value ,new-value)
  #+allegro
  `(excl:atomic-conditional-setf ,place ,new-value ,old-value)
  #-(or allegro lispworks ccl sbcl) `(error "Not supported."))

(defmacro atomic-setf (place new-value-function)
  (with-gensyms (func old-value new-value)
    `(loop :with ,func = ,new-value-function
        :for ,old-value = ,place
        :for ,new-value = (funcall ,func ,old-value)
        :until (compare-and-swap ,place ,old-value ,new-value)
        :finally (return ,new-value))))

(defmacro atomic-incf (place &optional (increment 1))
  (with-gensyms (old-value)
    (once-only (increment)
      `(atomic-setf ,place (lambda (,old-value) (+ ,old-value ,increment))))))

(defmacro atomic-decf (place &optional (decrement 1))
  `(atomic-incf ,place (- ,decrement)))

(defmacro atomic-push (value place)
  (with-gensyms (old-value)
    (once-only (value)
      `(atomic-setf ,place
                    (lambda (,old-value)
                      (cons ,value ,old-value))))))

(defmacro atomic-pop (place)
  (with-gensyms (old-value car)
    `(let (,car)
       (atomic-setf ,place (lambda (,old-value)
                             (setf ,car ,old-value)
                             (cdr ,old-value)))
       ,car)))

(defmacro without-interrupts (&body body)
  #+sbcl
  `(sb-sys:without-interrupts ,@body)
  #+ccl
  `(ccl:without-interrupts ,@body)
  #-(or ccl sbcl)
  (error "NOT SUPPORTED"))

(defmacro with-interrupts (&body body)
  #+sbcl
  `(sb-sys:allow-with-interrupts
    (sb-sys:with-interrupts
      ,@body))
  #+ccl
  `(ccl:with-interrupts-enabled ,@body)
  #-(or ccl sbcl)
  (error "NOT SUPPORTED"))

(defstruct light-queue
  spinlock
  (queue (cons nil nil)))

(defun lock-q (lq)
  (loop until (compare-and-swap (light-queue-spinlock lq) nil t)))

(defun unlock-q (lq)
  (setf (light-queue-spinlock lq) nil))

(defmacro with-queue-lock ((lq) &body body)
  (let ((q-var (gensym)))
    `(let ((,q-var ,lq))
       (unwind-protect
            (progn
              (lock-q ,q-var)
              ,@body)
         (unlock-q ,q-var)))))

(defun lq-enq (obj lq)
  (with-queue-lock (lq)
    (let ((q (light-queue-queue lq)))
      (if (null (car q))
          (setf (cdr q) (setf (car q) (list obj)))
          (setf (cdr (cdr q)) (list obj)
                (cdr q) (cdr (cdr q))))
      (car q))))

(defun lq-deq (lq)
  (with-queue-lock (lq)
    (let ((q (light-queue-queue lq)))
      (pop (car q)))))
