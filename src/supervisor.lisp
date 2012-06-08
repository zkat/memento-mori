(cl:defpackage #:hipocrite.supervisor
  (:use #:cl #:alexandria #:hipocrite.utils #:hipocrite)
  (:import-from #:hipocrite.server #:defcall #:defcast)
  (:nicknames #:hip-sup)
  (:export
   #:start-supervisor
   #:make-child-spec
   #:one-for-one))
(cl:in-package #:hipocrite.supervisor)

'one-for-one

(defstruct supervisor
  restart-strategy
  max-restarts
  max-restart-time
  recent-restarts
  (children (make-hash-table)))
(defstruct supervisor-child
  child-spec
  actor)
(defstruct (child-spec
             (:constructor
              make-child-spec (key
                               init-function
                               &key
                               (restart-on-completion-p t)
                               remove-after-shutdown-p
                               shutdown-timeout
                               supervisorp)))
  key
  init-function
  restart-on-completion-p
  remove-after-shutdown-p
  shutdown-timeout
  supervisorp)

(defun start-supervisor (restart-strategy
                         &key
                           linkp monitorp name debugp
                           (max-restarts 5) (max-restart-time 10)
                           initial-child-specs)
  (hip-srv:start
   (lambda ()
     (let ((table (make-hash-table)))
       (map nil (lambda (child-spec)
                  (setf (gethash (child-spec-key child-spec) table)
                        (make-supervisor-child :child-spec child-spec)))
            initial-child-specs)
       (make-supervisor
        :restart-strategy restart-strategy
        :max-restarts max-restarts
        :max-restart-time max-restart-time
        :children table)))
   :linkp linkp :monitorp monitorp :debugp debugp
   :trap-exits-p t :name name))

;;;
;;; Server callbacks
;;;
(defmethod hip-srv:on-init ((supervisor supervisor))
  (maphash-values (lambda (child)
                    (setf (supervisor-child-actor child)
                          (%start-child (supervisor-child-child-spec child))))
                  (supervisor-children supervisor)))

(defmethod hip-srv:on-message ((sup supervisor) (exit link-exit))
  (when-let (child (find (link-exit-linked-actor exit)
                         (hash-table-values (supervisor-children sup))
                         :key #'supervisor-child-actor))
    (format t "~&Supervisor child died: ~S. Restarting.~%" child)
    (setf (supervisor-child-actor child)
          (%start-child (supervisor-child-child-spec child)))))

(defcall count-children ()
    (supervisor supervisor)
  (hash-table-count (supervisor-children supervisor)))

(defcall list-children ()
    (supervisor supervisor)
  (hash-table-values (supervisor-children supervisor)))

(defun %start-child (child-spec)
  (funcall (child-spec-init-function child-spec)))

(defcall start-child (child-spec)
    (supervisor supervisor)
  (push
   (make-supervisor-child :child-spec child-spec
                          :actor (%start-child child-spec))
   (supervisor-children supervisor))
  t)

(defcall terminate-child (child)
    (supervisor supervisor)
  ;; TODO
  child)

(defcall restart-child (child)
    (supervisor supervisor)
  ;; TODO
  child)
