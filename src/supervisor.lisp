(cl:defpackage #:memento-mori.supervisor
  (:use #:cl #:alexandria #:memento-mori.utils #:memento-mori)
  (:import-from #:memento-mori.server #:defcall #:defcast)
  (:nicknames #:mori-sup)
  (:export
   #:start-supervisor
   #:make-child-spec))
(cl:in-package #:memento-mori.supervisor)

(defstruct supervisor
  max-restarts
  max-restart-time
  restarts
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

(defun start-supervisor (&key
                           linkp monitorp name debugp
                           (max-restarts 5) (max-restart-time 10)
                           initial-child-specs)
  (mori-srv:start
   (lambda ()
     (let ((table (make-hash-table)))
       (map nil (lambda (child-spec)
                  (setf (gethash (child-spec-key child-spec) table)
                        (make-supervisor-child :child-spec child-spec)))
            initial-child-specs)
       (make-supervisor
        :max-restarts max-restarts
        :max-restart-time max-restart-time
        :children table)))
   :linkp linkp :monitorp monitorp :debugp debugp
   :trap-exits-p t :name name))

;;;
;;; Server callbacks
;;;
(defmethod mori-srv:on-init ((supervisor supervisor))
  (maphash-values (lambda (child)
                    (setf (supervisor-child-actor child)
                          (%start-child (supervisor-child-child-spec child))))
                  (supervisor-children supervisor)))

(defun add-restart (supervisor)
  (let ((now (now)))
    (setf (supervisor-restarts supervisor)
          (cons now
                (loop
                   for restart in (supervisor-restarts supervisor)
                   while (in-period-p
                          restart
                          now
                          (supervisor-max-restart-time supervisor))
                   collect restart)))))

(defun in-period-p (from to period)
  (< (- to from) period))

(defmethod mori-srv:on-message ((sup supervisor) (exit link-exit))
  (when-let (child (find (link-exit-linked-actor exit)
                         (hash-table-values (supervisor-children sup))
                         :key #'supervisor-child-actor))
    (if (> (length (add-restart sup)) (supervisor-max-restarts sup))
        (signal 'actor-shutdown :reason "Too many restarts.")
        (setf (supervisor-child-actor child)
              (%start-child (supervisor-child-child-spec child))))))

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

(defun now ()
  (/ (get-internal-real-time) internal-time-units-per-second))
