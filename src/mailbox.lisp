(cl:defpackage #:hipocrite.mailbox
  (:use :cl)
  (:export #:make-mailbox
           #:mailbox-count
           #:send
           #:receive
           #:receive-if
           #:receive-cond
           #:receive-if-not))
(in-package #:hipocrite.mailbox)

#-sbcl
(defstruct mailbox
  cond-var
  headlock
  taillock
  )

#+sbcl
(defstruct mailbox
  (mbox (sb-concurrency:make-mailbox)))

(defmethod print-object ((mailbox mailbox) stream)
  (print-unreadable-object (mailbox stream :type t :identity t)
    (format stream "[~D messages]" (mailbox-count mailbox))))

(defun mailbox-count (mailbox)
  #-sbcl (error "TODO")
  #+sbcl (sb-concurrency:mailbox-count (mailbox-mbox mailbox)))

(defun send (mailbox obj)
  #-sbcl (error "TODO")
  #+sbcl
  (sb-concurrency:send-message (mailbox-mbox mailbox) obj))

(defun receive (mailbox &key timeout on-timeout)
  #-sbcl (error "TODO")
  (multiple-value-bind (result completedp)
      (sb-concurrency:receive-message
       (mailbox-mbox mailbox)
       :timeout timeout)
    (if completedp
        (values result t)
        (values (when on-timeout
                  (funcall on-timeout))
                nil))))

;; (defun receive-if (predicate mailbox &key timeout)
;;   #-sbcl (error "TODO")
;;   #+sbcl (error "TODO"))

;; (defmacro receive-cond ((mailbox &key timeout) &body clauses)
;;   #-sbcl (error "TODO")
;;   #+sbcl (error "TODO"))

;; (defun receive-if-not (predicate mailbox &key timeout)
;;   (receive-if (complement predicate) mailbox :timeout timeout))
