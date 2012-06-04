(cl:defpackage #:hipocrite.supervisor
  (:use #:cl #:hipocrite.utils #:hipocrite))
(cl:in-package #:hipocrite.supervisor)

(defun count-children (supervisor)
  ;; TODO
  supervisor)

(defun start-child (supervisor child-spec)
  ;; TODO
  (list supervisor child-spec))

(defun terminate-child (supervisor child-name)
  ;; TODO
  (list supervisor child-name))

(defun restart-child (supervisor child-name)
  ;; TODO
  (list supervisor child-name))

(defun list-children (supervisor)
  ;; TODO
  (list supervisor))
