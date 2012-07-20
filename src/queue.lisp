;;;; Lock-free FIFO queues, from "An Optimistic Approach to Lock-Free FIFO
;;;; Queues" by Edya Ladan-Mozes and Nir Shavit.
;;;;
;;;; Written by Nikodemus Siivola for SBCL.
;;;;
;;;; Adapted by Kat Marchán to include CCL support.
;;;;
;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was written at
;;;; Carnegie Mellon University and released into the public domain. The
;;;; software is in the public domain and is provided with absolutely no
;;;; warranty. See the COPYING and CREDITS files for more information.

(cl:defpackage #:memento-mori.queue
  (:use #:cl #:memento-mori.utils)
  (:export #:make-queue
           #:enqueue
           #:dequeue
           #:list-queue-contents
           #:queue-count
           #:queue-empty-p))
(cl:in-package #:memento-mori.queue)

(defconstant +dummy+ '.dummy.)

(declaim (inline make-node))
(defstruct node
  value
  (prev nil :type (or null node))
  (next nil :type (or null node)))

(declaim (inline %make-queue))
(defstruct (queue (:constructor %make-queue (head tail name))
                  (:copier nil)
                  (:predicate queuep))
  "Lock-free thread safe FIFO queue.

Use ENQUEUE to add objects to the queue, and DEQUEUE to remove them."
  (head (error "No HEAD."))
  (tail (error "No TAIL."))
  (name nil))

(setf (documentation 'queuep 'function)
      "Returns true if argument is a QUEUE, NIL otherwise."
      (documentation 'queue-name 'function)
      "Name of a QUEUE. Can be assingned to using SETF. Queue names
can be arbitrary printable objects, and need not be unique.")

(defun make-queue (&key name initial-contents)
  "Returns a new QUEUE with NAME and contents of the INITIAL-CONTENTS
sequence enqueued."
  (let* ((dummy (make-node :value +dummy+))
         (queue (%make-queue dummy dummy name)))
    (flet ((enc-1 (x)
             (enqueue x queue)))
      (declare (dynamic-extent #'enc-1))
      (map nil #'enc-1 initial-contents))
    queue))

(defun enqueue (value queue)
  "Adds VALUE to the end of QUEUE. Returns VALUE."
  (let ((node (make-node :value value)))
    (loop for tail = (queue-tail queue)
       do (setf (node-next node) tail)
         (when (compare-and-swap (queue-tail queue) tail node)
           (setf (node-prev tail) node)
           (return value)))))

(defun dequeue (queue)
  "Retrieves the oldest value in QUEUE and returns it as the primary value,
and T as secondary value. If the queue is empty, returns NIL as both primary
and secondary value."
  (tagbody
   :continue
     (let* ((head (queue-head queue))
            (tail (queue-tail queue))
            (first-node-prev (node-prev head))
            (val (node-value head)))
       ;; NOTE - no, I don't actually have any idea what this does, but I'm
       ;;        hoping for the best for non-SBCL implementations. - Kat Marchán
       #+sbcl
       (sb-thread:barrier (:read))
       (when (eq head (queue-head queue))
         (cond ((not (eq val +dummy+))
                (if (eq tail head)
                    (let ((dummy (make-node :value +dummy+ :next tail)))
                      (when (compare-and-swap (queue-tail queue)
                                              tail dummy)
                        (setf (node-prev head) dummy))
                      (go :continue))
                    (when (null first-node-prev)
                      (fix-list queue tail head)
                      (go :continue)))
                (when (compare-and-swap (queue-head queue)
                                        head first-node-prev)
                  ;; These assignment is not present in the paper, but are
                  ;; equivalent to the free(head.ptr) call there.
                  ;;
                  ;; First we unlink the HEAD from the queue -- the code in
                  ;; the paper leaves the dangling pointer in place.
                  ;;
                  ;; Then we NIL out the slots in HEAD to help the GC,
                  ;; otherwise conservativism might lead to massive chains of
                  ;; nodes being retained.
                  (setf (node-next first-node-prev) nil
                        (node-prev head) nil
                        (node-next head) nil
                        (node-value head) nil)
                  (return-from dequeue (values val t))))
               ((eq tail head)
                (return-from dequeue (values nil nil)))
               ((null first-node-prev)
                (fix-list queue tail head)
                (go :continue))
               (t
                (when (compare-and-swap (queue-head queue)
                                        head first-node-prev)
                  head)))))
     (go :continue)))

(defun fix-list (queue tail head)
  (let ((current tail))
    (loop while (and (eq head (queue-head queue)) (not (eq current head)))
          do (let ((next (node-next current)))
               (when (not next)
                 (return-from fix-list nil))
               (let ((next-node-prev (node-prev next)))
                 (when (not (eq next-node-prev current))
                   (setf (node-prev next) current))
                 (setf current next))))))

(defun list-queue-contents (queue)
  "Returns the contents of QUEUE as a list without removing them from the
QUEUE. Mainly useful for manual examination of queue state, as the list
may be out of date by the time it is returned."
  (let (all)
    (labels ((walk (node)
               ;; Since NEXT pointers are always right, traversing from tail
               ;; to head is safe.
               (let ((value (node-value node))
                     (next (node-next node)))
                 (unless (eq +dummy+ value)
                   (push value all))
                 (when next
                   (walk next)))))
      (walk (queue-tail queue)))
    all))

(defun queue-count (queue)
  "Returns the number of objects in QUEUE. Mainly useful for manual
examination of queue state, and in PRINT-OBJECT methods: inefficient as it
must walk the entire queue."
  (let ((n 0))
    (declare (unsigned-byte n))
    (labels ((walk (node)
               (let ((value (node-value node))
                     (next (node-next node)))
                 (unless (eq +dummy+ value)
                   (incf n))
                 (when next
                   (walk next)))))
      (walk (queue-tail queue))
      n)))

(defun queue-empty-p (queue)
  "Returns T if QUEUE is empty, NIL otherwise."
  (let* ((head (queue-head queue))
         (tail (queue-tail queue))
         (val (node-value head)))
    (and (eq head tail) (eq val +dummy+))))
