(in-package :wiktionary-bot)

(defmacro defun-task ((name varlist &body body) (cancel-name &body cancel-body))
  (let ((sym (gensym "CANCEL-TOKEN"))
	(fsym (gensym "FUNCTION-SYM")))
    `(let ((,sym nil))
       (defun ,name ,varlist
	 (labels ((reschedule-after (delay)
		    (schedule #',fsym delay))
		  (,fsym ()
		    (when ,sym
		      ,@cancel-body
		      (return-from ,fsym))
		    ,@body))
	   (setf ,sym nil)
	   (schedule #',fsym 0)))
       (defun ,cancel-name ()
	 (setf ,sym t)))))

(define-condition emergency-stop (error)
  ())

(defun-task
    (start-test-fail-loop (iterations)
     (when (zerop (decf iterations))
       (log-test 'test-fail-loop
		 "test fail loop, failing")
       (test-fail-interestingly))
     (log-test 'test-fail-loop
	       "test fail loop, countdown to failure: ~a"
	       iterations)
     (reschedule-after 1.0))
    (stop-test-fail-loops
     (log-test 'test-fail-loop
	       "test fail loop quitting")))

(defun-task
    (start-test-queue-loop (text &optional (delay 1.0))
     (log-detail 'test-queue-loop
		 "test queue loop (~a at ~a)"
		 text
		 delay)
     (reschedule-after delay))
    (stop-test-queue-loops
     (log-detail 'test-queue-loop
		"test queue loop (~a at ~a) quitting"
		text
		delay)))

(defun-task
    (start-emergency-page-task (&key (interval 60))
      (log-info 'emergency-page-task
		"checking for emergency stop, next in ~a seconds"
		interval)
      (when (emergency-stop?)
	(error 'emergency-stop))
      (reschedule-after interval))
    (stop-emergency-page-tasks))

(defun start-collect-media-task (&key (languages '(:swedish)) (interval (* 60 30)))
  (run-continuation-task (make-fetch-from-standard-front-pages-continuation :languages languages)
			 :repeat-interval interval))

#+fdjsk									
(defun-task
    (start-collect-media-task (&key (languages '(:swedish)) (interval (* 60 30)))
      (fetch-from-standard-front-pages :languages languages)
      (log-info 'media-collect-task
		"next media collection in ~a seconds"
		interval)
      (reschedule-after interval))
    (stop-collect-media-tasks))