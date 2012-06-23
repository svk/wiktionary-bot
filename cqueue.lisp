(in-package :wiktionary-bot)

(defparameter *task-pq* (create-heap))
(defparameter *timer-resolution* 1.0)

(defun clear-task-queue ()
  (clear-heap *task-pq*))

(defun schedule (task &optional (delay 0))
  (add-to-heap *task-pq* (+ (get-universal-time) (rational delay)) task))

(defun test-fail-interestingly ()
  (with-open-file (f "/tmp/nonexistent" :direction :input :if-does-not-exist :error)
    (+ 432 (/ 1 0))))

(defun idle-task ()
  (sleep *timer-resolution*))

(defun run-task-queue ()
  (let ((queued-tasks))
    (loop
       :do (let ((task (or (pop queued-tasks)
			   (pop-from-heap *task-pq* :priority-limit (get-universal-time))
			   #'idle-task)))
	     (restart-case (funcall task)
	       (delay-task ()
		 :report (lambda (stream)
			   (format stream "Reschedule task ~a with delay" task))
		 (schedule task 60))
	       (retry-task ()
		 :report (lambda (stream)
			     (format stream "Retry task ~a" task))
		 (push task queued-tasks))
	       (skip-task ()
		 :report (lambda (stream)
			     (format stream "Skip task ~a" task))))))))

(defvar *task-queue-process-lock* (mp:make-process-lock))
(defvar *task-queue-process* nil)

(defun kill-task-queue ()
  (with-lock *task-queue-process-lock*
    (when *task-queue-process*
      (mp:process-kill *task-queue-process* :wait t)
      (setf *task-queue-process* nil))))

(defun start-task-queue (&key kill)
  (with-lock *task-queue-process-lock*
    (when *task-queue-process*
      (if kill
	  (kill-task-queue)
	  (return-from start-task-queue nil)))
    (setf *task-queue-process* (mp:process-run-function "task queue" #'run-task-queue))))

(defun run-continuation-task (task &key repeat-interval)
  (labels ((repeat-or-stop ()
	     (log-detail 'run-continuation-task
			 "task ~a finished, will ~a"
			 task
			 (if repeat-interval
			     (format nil "repeat in ~a seconds" repeat-interval)
			     "now exit"))
	     (when repeat-interval
	       (schedule #'(lambda ()
			     (run-continuation-task task :repeat-interval repeat-interval))
			 repeat-interval)))
	   (schedule-next (delay future)
	     (log-detail 'run-continuation-task
			 "scheduling task ~a in ~a seconds"
			 future
			 delay)
	     (schedule future delay)))
    (schedule #'(lambda ()
		  (funcall task
			   #'schedule-next
			   #'repeat-or-stop))
	      0)))

		