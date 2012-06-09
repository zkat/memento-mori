(cl:defpackage #:memento-mori.example.lyse-musicians
  (:use #:cl #:alexandria #:memento-mori))
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
(defstruct musician role name skill)

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
  (mori-timer:send-after (random 3.0) 'play))

(defun pick-name ()
  (concatenate 'string
               (random-elt '("Valerie" "Arnold" "Carlos" "Dorothy" "Keesha"
                             "Phoebe" "Ralphie" "Tim" "Wanda" "Janet"))
               " "
               (random-elt '("Frizzle" "Perlstein" "Ramon" "Ann" "Franklin"
                             "Terese" "Tennelli" "Jamal" "Li" "Persteil"))))

(mori-srv:defcall stop-musician ()
    (musician musician)
  (mori-srv:exit-server-loop))

(defmethod mori-srv:on-message ((musician musician) (message (eql 'play)))
  (let ((name (musician-name musician)))
    (case (musician-skill musician)
      (good (report "~a sounded good!" name))
      (bad (cond ((= 1 (random 5))
                  (report "~a played a false note. Uh oh." name)
                  (mori-srv:exit-server-loop 'bad-note))
                 (t
                  (report "~a produced sound!" name)))))
    (mori-timer:send-after (random 0.75) 'play)))

(defmethod mori-srv:on-message ((musician musician) (message link-exit))
  (report "The band supervisor walked out on ~a!" (musician-name musician))
  (mori-srv:exit-server-loop 'supervisor-quit))

(defmethod mori-srv:on-shutdown ((musician musician) reason)
  (report "~a going away because of ~a (~a)"
          (musician-name musician) reason (musician-role musician)))

;;;
;;; Supervisor
;;;
(defun start-band-supervisor ()
  (mori-sup:start-supervisor
   :name 'band-supervisor
   :initial-child-specs
   (list
    (mori-sup:make-child-spec 'singer (curry #'start-musician 'singer 'good))
    (mori-sup:make-child-spec 'bass (curry #'start-musician 'bass 'good))
    (mori-sup:make-child-spec 'drum (curry #'start-musician 'drum 'bad))
    (mori-sup:make-child-spec 'keytar (curry #'start-musician 'keytar 'good)))))
