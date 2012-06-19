(cl:defpackage #:memento-mori.example.misc-actor
  (:use #:cl #:alexandria #:memento-mori #:memento-mori.utils))
(cl:in-package #:memento-mori.example.misc-actor)

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
           (loop for exit = (receive :timeout 1 :on-timeout (constantly nil))
              while (remote-exit-p exit)
              do (format t "~&Got an exit message with reason: ~s~%"
                         (remote-exit-reason exit)))
           (format t "~&Done. Exiting master actor.~%"))
         :trap-exits-p t))

(defun test-chain (n)
  (with-actor-context (:trap-exits-p t)
    (format t "~&Chain has died with reason: ~s~%" (chain n))))

(defun chain (n)
  (cond ((= n 0)
         #+nil(exit "wat")
         (receive :timeout 2
                  :on-timeout
                  (lambda ()
                    (error "I can't take this anymore."))))
        (t
         (format t "~&Spawning process #~a and waiting.~%" n)
         (spawn (lambda ()
                  (chain (1- n)))
                :linkp t)
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

(defun error-logging ()
  (enable-crash-logging)
  (spawn (lambda ()
           (error "OH THE HUMANITY"))))

(defun timeout ()
  (spawn (lambda ()
           (receive :timeout 0.5
                    :on-timeout
                    (lambda ()
                      (print "Timed out."))))
         :debugp t))

(defun monitors ()
  (spawn (lambda ()
           (spawn (lambda ()
                    (exit 'dying))
                  :monitorp t)
           (let ((exit (receive)))
             (when (monitor-exit-p exit)
               (format t "~&Monitor signaled exit: ~a. Reason: ~s.~%"
                       (monitor-exit-monitor exit)
                       (monitor-exit-reason exit)))))))

(defun test-selective-receive ()
  (with-actor-context ()
    (let ((self (current-actor)))
      (spawn (curry #'send self 'second))
      (sleep 1)
      (spawn (curry #'send self 'first))
      (receive-cond (msg)
        ((eq msg 'first)
         (print "Got the first message")))
      (receive-cond (msg)
        ((eq msg 'anything-else)
         (print "Got something unexpected."))
        (after 5
               (print "Timed out waiting for anything-else.")))
      (print
       (multiple-value-list
        (receive-cond (msg)
          ((eq msg 'second)
           (values "Got the second message" 'and-another-value))))))))

(defun test-timers ()
  (spawn (lambda ()
           (mori-timer:call-after 1 (curry #'print "test"))
           (loop (sleep 1)))))

(defun test-remote-exits ()
  (let ((killme (spawn (lambda ()
                         (handler-case
                             (loop (sleep 0.5))
                           (exit (e)
                             (mori-log:error "I should not have caught ~a!" e)))))))
    ;; Remote exits are uncatchable. You *must* trap exits.
    (exit 'die killme)))

(defun test-dead-actor-monitoring ()
  (let ((dead (spawn (lambda () "goodbye, cruel world!"))))
    (sleep 1)
    (spawn (lambda ()
             (monitor dead)
             (mori-log:info "Down signal: ~A" (receive :timeout 5))))))

(defun test-dead-actor-linking ()
  (let ((dead (spawn (lambda () "goodbye, cruel world!"))))
    (sleep 1)
    (spawn (lambda ()
             (link dead)
             (mori-log:info "First: ~A" (receive :timeout 5))))
    (spawn (lambda ()
             (mori-log:info "Linking to ~a" dead)
             (link dead)
             (mori-log:info "Second: ~A" (receive :timeout 5)))
           :trap-exits-p t)))
