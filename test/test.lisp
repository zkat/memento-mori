(in-package #:hipocrite)

(defun test ()
  (let ((actor (spawn (lambda ()
                        (print (receive))))))
    (send actor "Hello")
    (sleep 0.5)
    (print (actor-alive-p actor))))

(defun test2 ()
  (let* ((thing1 (spawn (lambda ()
                          (let ((thing2 (receive)))
                            (format t "~&Got thing2: ~a~%" thing2)
                            (send thing2 (current-actor))))))
         (thing2 (spawn (lambda ()
                          (format t "~&Got thing1: ~a~%" (receive))))))
    (format t "~&Thing1: ~a, Thing2: ~a" thing1 thing2)
    (send thing1 thing2)))

(defun test-links ()
  (spawn (lambda ()
           (loop repeat 10 do
                (spawn (lambda () "hi")
                       :linkp t))
           (loop repeat 10 do
                (let ((exit (receive)))
                  (format t "~&Got an exit from linked actor ~a. Reason: ~s~%"
                          (actor-exit-actor exit)
                          (actor-exit-reason exit))))
           (format t "~&Done. Exiting master actor.~%"))
         :trap-exits-p t))

(defun test-chain (n)
  (spawn (lambda ()
           (chain n)
           (format t "~&Chain has died with reason: ~s~%" (receive)))
         :trap-exits-p t))

(defun chain (n)
  (cond ((= n 0)
         (error "Goodbye, cruel world"))
        (t
         (format t "~&Spawning new process and waiting.~%")
         (spawn (lambda ()
                  (chain (1- n)))
                :linkp t
                :debugp t)
         (receive))))

(defun errors ()
  (spawn (lambda ()
           (spawn (lambda ()
                    (error "fail"))
                  :linkp t
                  :debugp t)
           (print (receive)))
         :trap-exits-p t
         :debugp t)
  (let ((*debug-on-error-p* t))
    (spawn (lambda ()
             (error "Blech")))))

(defun timeout ()
  (spawn (lambda ()
           (receive :timeout 0.5
                    :on-timeout
                    (lambda ()
                      (print "Timed out."))))
         :debugp t))
