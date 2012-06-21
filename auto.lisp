(in-package :wiktionary-bot)

(defparameter *stop-page* "User:MetallmanulBot/Stopp")
(defparameter *stop-pattern* (cl-ppcre:create-scanner "STOP" :case-insensitive-mode t))

(defparameter *stop-page-begin-status* "<!-- META-BEGIN-FIELD-STATUS -->")
(defparameter *stop-page-end-status* "<!-- META-END-FIELD-STATUS -->")

(defparameter *stop-page-line-pattern* (cl-ppcre:create-scanner "----"))

(defparameter *job-description* nil)
(defparameter *job-started* nil)

(defparameter *success-reason* "fullført")
(defparameter *emergency-stop-reason* "avbrutt via stoppside")
(defparameter *error-reason* "avbrutt manuelt eller pga. feil")

(defun after-last-match (needle haystack)
  (let ((position 0))
    (loop :do (multiple-value-bind (begin end)
		  (cl-ppcre:scan needle haystack :start position)
		(when (null begin)
		  (return-from after-last-match (if (> position 0)
						    (subseq haystack position)
						    nil)))
		(setf position end)))))

(defun emergency-stop? ()
  (cl-ppcre:scan *stop-pattern*
		 (after-last-match *stop-page-line-pattern*
				   (page-source *stop-page*))))

(defun update-auto-status (new-status)
  (labels ((mutator (old-source)
	     (multiple-value-bind (begin-begins begin-ends)
		 (cl-ppcre:scan *stop-page-begin-status* old-source)
	       (declare (ignore begin-begins))
	       (multiple-value-bind (end-begins end-ends)
		   (cl-ppcre:scan *stop-page-end-status* old-source)
		 (declare (ignore end-ends))
		 (concatenate 'string
			      (subseq old-source 0 begin-ends)
			      new-status
			      (subseq old-source end-begins))))))
    (api-edit *stop-page*
	      "oppdaterer botstatus"
	      :minor t
	      :mutator #'mutator)))

(defun update-auto-status-begin-job ()
  (assert (and *job-description*
	       *job-started*))
  (update-auto-status (format nil "
'''''Ja'''''.

Pågående jobb: '''~a'''

Startet: ~a

" *job-description* (rfc3339:make-timestamp :utc-time *job-started*))))

(defun update-auto-status-end-job (reason)
  (assert (and *job-description*
	       *job-started*))
  (update-auto-status (format nil "
'''''Nei'''''.

Siste jobb: '''~a'''

Startet: ~a

Avsluttet: ~a (~a)
" *job-description* (rfc3339:make-timestamp :utc-time *job-started*)
                    (rfc3339:make-timestamp :utc-time (get-universal-time))
		    reason)))

(defmacro automatic-loop ((job-description &key (announce t) (wait 1) (report t)) &body body)
  (let ((stop-reason-sym (gensym "STOP-REASON"))
	(report-sym (gensym "REPORT"))
	(announce-sym (gensym "ANNOUNCE"))
	(wait-sym (gensym "WAIT")))
    `(progn
       (assert (not *job-description*))
       (let ((*job-description* ,job-description)
	     (*job-started* (get-universal-time))
	     (,stop-reason-sym :ERROR)
	     (,report-sym ,report)
	     (,announce-sym ,announce)
	     (,wait-sym ,wait))
	 (unwind-protect
	      (progn
		(when ,announce-sym
		  (update-auto-status-begin-job))
		(loop	   
		   :do (progn ,@body)
		   :do (when (emergency-stop?)
			 (setf ,stop-reason-sym :EMERGENCY)
			 (return))
		   :do (when ,report-sym
			 (if (functionp ,report-sym)
			     (funcall ,report-sym)
			     (format
			      t
			      "[automatic-loop] ~a: job (~a) ongoing since ~a~%"
			      (rfc3339:make-timestamp)
			      *job-description*
			      (rfc3339:make-timestamp :utc-time *job-started*))))
		   :do (when ,wait-sym
			 (sleep ,wait-sym))))
		(unless (eq ,stop-reason-sym :EMERGENCY)
		  (setf ,stop-reason-sym :SUCCESS)))
	 (when ,announce-sym
	   (update-auto-status-end-job
	    (case ,stop-reason-sym
	      (:EMERGENCY *emergency-stop-reason*)
	      (:SUCCESS *success-reason*)
	      (:ERROR *error-reason*)
	      (otherwise "?"))))
	 nil))))

(defun auto-exhaust-conjugation-tasks (&key (job-description "bøyningsformsartikler") (announce t))
  (assert (or (not announce)
	      (> (length *collected-conjugation-tasks*) 50)))
  (automatic-loop (job-description
		   :wait 2
		   :report t
		   :announce announce)
    (when (null *collected-conjugation-tasks*)
      (return))
    (simple-swedish-conjugation-task (pop *collected-conjugation-tasks*))))

(defun full-auto-conjugation-tasks (&key (job-description "bøyningsformsartikler (helautomatisk)"))
  (let ((*collected-conjugation-tasks* nil))
    (automatic-loop (job-description
		     :wait 2
		     :report t)
      (if (null *collected-conjugation-tasks*)
	  (collect-some-conjugation-tasks)
	  (simple-swedish-conjugation-task (pop *collected-conjugation-tasks*))))))

(defun full-auto-conjugation-tasks+recent-conjugation (&key (job-description "bøyningsformsartikler (helautomatisk, med overvåkning av Recent Changes)"))
  (let ((*collected-conjugation-tasks* nil)
	(last-rc-check nil)
	(rc-interval (* 60 10))
	(error-interval 60))
    (automatic-loop (job-description
		     :wait 0.5
		     :report t)
      (handler-case
	  (if (or (null last-rc-check)
		  (>= (- (get-universal-time) last-rc-check)
		      rc-interval))
	      (let ((changed-titles (remove-duplicates (recent-changes :n 50 :end-timestamp last-rc-check)
						       :test #'equal)))
		(collect-conjugation-tasks changed-titles)
		(setf *collected-conjugation-tasks*
		      (reverse (filter-redlink-list *collected-conjugation-tasks* :key #'second)))
		(setf last-rc-check (get-universal-time))
		(irc-report (concatenate 'string "Checked RC, tasks now:" (funcall #'string-join " " (mapcar #'second *collected-conjugation-tasks*)))))
	      (if (null *collected-conjugation-tasks*)
		  (progn (collect-some-conjugation-tasks)
			 (irc-report (concatenate 'string "Collected tasks, tasks now:" (funcall #'string-join " " (mapcar #'second *collected-conjugation-tasks*)))))
		  #-newcode
		  (progn (simple-swedish-conjugation-tasks *collected-conjugation-tasks*)
			 (setf *collected-conjugation-tasks* nil))
		  #+oldcode(loop
		     :for i :from 1 :to 10
		     :while *collected-conjugation-tasks*
		     :do (simple-swedish-conjugation-task (pop *collected-conjugation-tasks*)))))
	(operation-failed ()
	  (format t "[auto] operation failed, delay ~d seconds~%" error-interval)
	  (sleep error-interval)
	  (setf error-interval (* 2 error-interval)))))))
    