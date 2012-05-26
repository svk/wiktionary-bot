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

(defmacro automatic-loop ((job-description &key (wait 1) (report t)) &body body)
  (let ((stop-reason-sym (gensym "STOP-REASON")))
    `(progn
       (assert (not *job-description*))
       (let ((*job-description* ,job-description)
	     (*job-started* (get-universal-time))
	     (,stop-reason-sym :ERROR))
	 (unwind-protect
	      (progn
		(update-auto-status-begin-job)
		(loop	   
		   :do (progn ,@body)
		   :do (when (emergency-stop?)
			 (setf ,stop-reason-sym :EMERGENCY)
			 (return))
		   ,@(when report
			   (list :do
				 (if (functionp report)
				     (list 'funcall report)
				     '(format
				       t
				       "[automatic-loop] ~a: job (~a) ongoing since ~a~%"
				       (rfc3339:make-timestamp)
				       *job-description*
				       (rfc3339:make-timestamp :utc-time *job-started*)))))
		   ,@(when wait (list :do `(sleep ,wait))))
		(setf ,stop-reason-sym :SUCCESS))
	   (update-auto-status-end-job
	    (case ,stop-reason-sym
	      (:EMERGENCY *emergency-stop-reason*)
	      (:SUCCESS *success-reason*)
	      (:ERROR *error-reason*)
	      (otherwise "?"))))))))

(defun auto-exhaust-conjugation-tasks (&key (job-description "lager bøyningsformsartikler"))
  (automatic-loop (job-description
		   :wait 10
		   :report t)
    (when (null *collected-conjugation-tasks*)
      (return))
    (simple-swedish-conjugation-task (pop *collected-conjugation-tasks*))))
    
    