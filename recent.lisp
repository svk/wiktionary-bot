(in-package :wiktionary-bot)

(defvar *last-recent-processed* nil)

(defun lisp-time->linux-time (&optional (x (get-universal-time)))
  (- x (encode-universal-time 0 0 0 1 1 1970 0)))

(defun recent-changes (&key (n 50) new-only (end-timestamp *last-recent-processed*) allow-bot)
  (let ((entries (extract '(:query :recentchanges)
			  (apply #'api
				 :action :query
				 :list :recentchanges
				 :recentprop '(:title)
				 :rcshow (append (unless allow-bot
						   (list :!bot)))
				 :rcexcludeuser "MetallmanulBot"
				 :rclimit (format nil "~d" n)
				 :rcnamespace "0"
				 (append (when end-timestamp
					     (list :rcend
						   (format nil "~d" (lisp-time->linux-time end-timestamp))
						   #+oops
						   (rfc3339:xml-rpc-timestamp
						    (rfc3339:make-timestamp :utc-time end-timestamp))))
					 (when new-only
					   (list :rctype "new")))))))
    (values (mapcar (papply (extract '(:title) ?))
		    entries)
	    (when entries
	      (rfc3339:utc-time-of (rfc3339:parse-string (extract '(:timestamp) (car entries))))))))


(defmacro patrolling-recent-changes (title-var (&key (delay 60) (n 50) (batch nil)) &body body)
  (let ((since-sym (gensym "SINCE"))
	(new-since-sym (gensym "NEW-SINCE"))
	(titles-sym (gensym "TITLES")))
    `(let ((,since-sym nil))
       (loop
	  :do (multiple-value-bind (,titles-sym ,new-since-sym)
		  (recent-changes :n ,n :end-timestamp ,since-sym)
		(when ,new-since-sym
		  (setf ,since-sym ,new-since-sym))
		,(if batch
		     `(let ((,title-var ,titles-sym))
			,@body)
		     `(dolist (,title-var ,titles-sym)
			,@body))
		,@(if delay
		      (list `(sleep ,delay))
		      nil))))))

(defun patrol-recent-changes-for-conjugation ()
  (patrolling-recent-changes titles
      (:n 10 :batch t)
    (let ((*collected-conjugation-tasks* nil))
      (collect-conjugation-tasks titles)
      (let ((*collected-conjugation-tasks*
	     (filter-redlink-list *collected-conjugation-tasks* :key #'second)))
	(loop
	   :while *collected-conjugation-tasks*
	   :do (simple-swedish-conjugation-task (pop *collected-conjugation-tasks*)))))))
