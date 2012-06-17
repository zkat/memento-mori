(cl:defpackage #:memento-mori.example.lyse-musicians
  (:use #:cl #:alexandria #:memento-mori)
  (:export
   #:start-musician
   #:stop-musician
   #:start-band-supervisor
   #:stop-band-supervisor))
(cl:in-package #:memento-mori.example.lyse-musicians)

;;; TODO - this doesn't quite work yet.
;;; Implementation of the band + band supervisor from
;;; http://learnyousomeerlang.com/supervisors#testing-it-out

;;;
;;; Utils
;;;
(defun report (format-string &rest format-args)
  (apply #'mori-log:info format-string format-args))

;;;
;;; Musician
;;;
(defstruct musician role name skill timer)

(defun start-musician (role skill)
  (mori-srv:start (lambda () (make-musician :role role :skill skill))
                  :linkp t
                  :name role
                  :trap-exits-p t))

(defmethod mori-srv:on-init ((musician musician))
  (setf *random-state* (make-random-state t)
        (musician-name musician) (pick-name))
  (report "Musician ~a, playing the ~a entered the room."
                 (musician-name musician)
                 (musician-role musician))
  (setf (musician-timer musician)
        (mori-timer:send-after (random 3) 'play)))

(defun pick-name ()
  (concatenate 'string
               (random-elt '("Valerie" "Arnold" "Carlos" "Dorothy" "Keesha"
                             "Phoebe" "Ralphie" "Tim" "Wanda" "Janet"))
               " "
               (random-elt '("Frizzle" "Perlstein" "Ramon" "Ann" "Franklin"
                             "Terese" "Tennelli" "Jamal" "Li" "Perstein"))))

(mori-srv:defcall stop-musician ()
    (musician musician)
  (mori-srv:exit-server-loop))

(defmethod mori-srv:on-message ((musician musician) (message (eql 'play)))
  (let ((name (musician-name musician)))
    (case (musician-skill musician)
      (good (report "~a sounded good!" name))
      (bad (cond ((= 1 (random 4))
                  (report "~a played a false note. Uh oh." name)
                  (exit 'bad-note))
                 (t
                  (report "~a produced sound!" name)))))
    ;; FIXME - It seems to be really easy to overload timers in CCL. Even
    ;; setting this to 0.5 puts a surprising amount of load on the system,
    ;; even though sleep + send does not. SBCL is perfectly happy with
    ;; small timeouts, though.
    (setf (musician-timer musician)
          (mori-timer:send-after 1 'play))))

(defmethod mori-srv:on-message ((musician musician) (remote-exit remote-exit))
  (report "The band  walked out on ~a!" (musician-name musician))
  (exit (remote-exit-reason remote-exit)))

(defmethod mori-srv:on-shutdown ((musician musician) reason)
  (report "~a going away because of ~a (~a)"
          (musician-name musician) reason (musician-role musician))
  ;; Clean up the timer when we shut down..
  (when-let (timer (musician-timer musician))
    (mori-timer:cancel-timer timer)))

;;;
;;; Supervisor
;;;
(defun start-band-supervisor ()
  ;; TODO - The LYSE example has multiple kinds of supervisor, but mori
  ;; currently only supports one-for-one.
  (mori-sup:start-supervisor
   :name 'band-supervisor
   :max-restarts 3 ; Number of restarts allowed...
   :max-restart-time 60 ; ...within this many seconds
   :initial-child-specs
   (flet ((child (role skill)
            (mori-sup:make-child-spec role (curry #'start-musician role skill))))
     (mapcar #'child '(singer bass drum keytar) '(good good bad good)))))

(defun stop-band-supervisor (&optional killp)
  (if killp
      (kill 'band-supervisor)
      (exit 'shutdown 'band-supervisor)))
