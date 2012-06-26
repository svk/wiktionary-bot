(in-package :wiktionary-bot)

(define-module-lock *conjugation-lock*
    with-conjugation
  defun-conjugation)

(defparameter *conjugation-check-log-filename* #p"./data/conjugation-checked-pages-log.generated.lisp")

(defparameter *conjugation-checked-pages* (let ((ht (make-hash-table :test #'equal)))
			       (with-open-file (f *conjugation-check-log-filename* :direction :input :if-does-not-exist nil)
				 (when f
				   (loop
				      :for x = (let ((*read-eval* nil)) (read f nil nil))
				      :until (null x)
				      :do (setf (gethash (first x) ht) (second x)))))
			       ht))

(defvar *collected-conjugation-tasks* nil)

(defun-conjugation register-in-conjugation-check-log (title timestamp)
  (setf (gethash title *conjugation-checked-pages*) timestamp)
  (with-open-file (*standard-output* *conjugation-check-log-filename* :direction :output :if-exists :append)
    (write (list title timestamp))))

(defun-conjugation in-conjugation-check-log? (title)
  (multiple-value-bind (timestamp present-p)
      (gethash title *conjugation-checked-pages*)
    (declare (ignore timestamp))
    present-p))

(let ((regex (cl-ppcre:create-scanner "{{böjning\\|sv\\|([^|}]+)\\|(text=)?[^}]+}}")))
  (defun-conjugation word-class-in-swedish-conjugation-link (source)
    (multiple-value-bind (match groups)
	(cl-ppcre:scan-to-strings regex source)
      (when match
	(return-from word-class-in-swedish-conjugation-link (aref groups 0))))))

(defun-conjugation print-swedish-word-classes ()
  (let ((results nil))
    (dolist (title (swedish-dump-titles :namespace "0"))
      (let ((result (word-class-in-swedish-conjugation-link (swedish-dump-text title))))
	(when (and result
		   (not (find result results :test #'equal)))
	  (push result results)
	  (format t "~a~%" result))))
    results))

(defun-conjugation count-swedish-word-classes ()
  (let ((results nil))
    (dolist (title (swedish-dump-titles :namespace "0"))
      (let ((result (word-class-in-swedish-conjugation-link (swedish-dump-text title))))
	(when result
	  (let ((present (find result results :test #'equal :key #'car)))
	    (if present
		(incf (cdr present))
		(push (cons result 1) results))))))
    results))


;; results:
;; (("räkn" . 1) ("substantiv" . 2) ("adjektiv" . 2) ("adv" . 7)
;; ("pron" . 25) ("adj" . 5424) ("verb" . 8671) ("subst" . 45483))
;; "adjektiv" etc. likely typos. "pron" is a closed class.
;; adverbs are usually not conjugated?
;; relevant are "adj" "verb" "subst"

(defun-conjugation swedish-conjugation-pos (type)
  (cond ((find type '(:noun :subst :substantiv))
	 "subst")
	((find type '(:adjective :adj :adjektiv))
	 "adj")
	((find type '(:verb))
	 "verb")
	(t (error (format nil "unrecognized Swedish conjugation POS: ~a" type)))))

(defun-conjugation create-swedish-conjugation-link (target type)
  (concatenate 'string
	       "{{böjning|sv|"
	       (swedish-conjugation-pos type)
	       "|"
	       target
	       "}}"))

(defun-conjugation swedish-pages-with-blessed-grammar-templates-in-dump (&key sample criterion)
  (let ((template-names (swedish-blessed-grammar-templates)))
    (log-detail 'swedish-pages-with-blessed-grammar-templates-in-dump "beginning scan")
    (let ((pages (remove-if-not
		       (or criterion
			   #'(lambda (title) (declare (ignore title)) t))
		       (if sample
			   (swedish-dump-titles-random-sample sample)
			   (swedish-dump-titles :namespace "0")))))
      (log-detail 'swedish-pages-with-blessed-grammar-templates-in-dump "scanning ~a pages" (length pages))
      (let ((result (loop
		       :for title :in pages
		       :for i :from 1
		       :do (when (zerop (mod i 100))
			     (log-detail 'swedish-pages-with-blessed-grammar-templates-in-dump
					 "processed ~a"
					 i))
		       :if (let ((source (swedish-dump-text title)))
			     (top-level-templates source :filter template-names))
		       :collect title)))
	(log-detail 'swedish-pages-with-blessed-grammar-templates-in-dump "done scanning, result ~a pages" (length result))
	result))))


(defun-conjugation create-swedish-conjugation-link-pattern (target type)
  (cl-ppcre:create-scanner (concatenate 'string
					"{{böjning\\|sv\\|"
					(swedish-conjugation-pos type)
					"\\|"
					target ;; obs
					"}}")))

(defun-conjugation parse-swedish-grammar-table (table)
  (let ((functions (list (cons #'parse-swedish-noun-table :noun)
			 (cons #'parse-swedish-verb-table :verb)
			 (cons #'parse-swedish-verb-table-alternative :verb)
			 (cons #'parse-swedish-verb-table-always-active :verb)
			 (cons #'parse-swedish-verb-table-always-active-alternative :verb)
			 (cons #'parse-swedish-noun-uncountable-table :noun)
			 (cons #'parse-swedish-adjective-noncomparative-table :adjective)
			 (cons #'parse-swedish-adjective-table :adjective))))
    (destructuring-dolist ((function . type) functions)
      (let ((result (funcall function table)))
	(when result
	  (return-from parse-swedish-grammar-table
	    (list result type)))))))

(let ((robotskapad-regex (cl-ppcre:create-scanner "{{robotskapad}}")))
  (defun-conjugation source-has-taboo? (source)
    (or (cl-ppcre:scan robotskapad-regex source))))

(defun-conjugation swedish-missing-conjugation-links (word)
  (when (in-conjugation-check-log? word)
    (return-from swedish-missing-conjugation-links nil))
  (register-in-conjugation-check-log word (get-universal-time))
  (collecting
    (destructuring-dolist ((conjugations pos)
			   (remove-if-not #'identity
					  (mapcar #'parse-swedish-grammar-table
						  (scan-for-grammar-tables (swedish-blessed-grammar-templates)
									   word
									   :early-warning t))))
      (destructuring-dolist ((conjugation conjugated-words) conjugations)
	(dolist (conjugated-word conjugated-words)
	  (unless (or (equal conjugated-word word)
		      (or (and (eql (length conjugated-word) 1)
			       (find (char-code (char conjugated-word 0))
				     '(9660 9650 8211) ;; down-arrow and up-arrow and some sort of dash
				     :test #'eql))
			  (or (search "," conjugated-word)
			      (search "(" conjugated-word)
			      (search ")" conjugated-word))
			  (equal conjugated-word "")))
	    (add-to-reverse-inflect-material word conjugated-word)
	    (let ((pattern (create-swedish-conjugation-link-pattern word pos)))
	      (unless (and (swedish-dump-text conjugated-word)
			   (cl-ppcre:scan pattern (swedish-dump-text conjugated-word)))
		(unless (or (source-has-taboo? (swedish-dump-text word))
			    (source-has-taboo? (swedish-dump-text conjugated-word)))
		  (collect (list pos conjugated-word word conjugation)))))))))))

(defun-conjugation collect-conjugation-tasks (words)
  (loop
     :for word :in words
     :for number :from 1
     :do (loop
	    :for task :in (swedish-missing-conjugation-links word)
	    :do (pushnew task *collected-conjugation-tasks* :test #'equal :key #'second))
     :do (format t "~a [collect-conjugation-tasks] processed #~a of ~a: ~a~%"
		 (rfc3339:make-timestamp)
		 number
		 (length words)
		 word)))

(defun-conjugation create-swedish-derivation-link (target derivation-type)
  (assert (find derivation-type '("adj" "verb" "prespart" "perfpart") :test #'equal))
  (concatenate 'string
	       "{{avledning|"
	       target
	       "|"
	       derivation-type
	       "}}"))

(defun-conjugation swedish-new-conjugated-participle-page (base-form grammar)
  (assert (find :participle grammar))
  (let ((derivation-type (cond ((find :present grammar) "prespart")
			       ((find :perfect grammar) "perfpart")
			       (t (error "swedish-new-conjugated-participle-page: inappropriate grammar")))))
    (format nil "==Svenska==
===~a===
'''{{subst:PAGENAME}}'''
#~a" (swedish-long-pos-name :adjective) (create-swedish-derivation-link base-form derivation-type))))

(defun-conjugation swedish-new-conjugated-form-page (base-form type)
  (format nil "==Svenska==
===~a===
'''{{subst:PAGENAME}}'''
#~a
" (swedish-long-pos-name type) (create-swedish-conjugation-link base-form type)))

(defun-conjugation swedish-long-pos-name (type)
  (case type
    (:adjective "Adjektiv")
    (:noun "Substantiv")
    (:verb "Verb")))

(defun-conjugation simulate-swedish-conjugation-task (task &key (test-page "User:MetallmanulBot/Grammar"))
  (destructuring-bind (type conjugated-word base-word grammar)
      task
    (let ((template (create-swedish-conjugation-link base-word type))
	  (template-pattern (create-swedish-conjugation-link-pattern base-word type))
	  (existing (page-source conjugated-word))
	  (language "Svenska")
	  (type-name (swedish-long-pos-name type)))
      (when (not (cl-ppcre:scan template-pattern existing))
	(let* ((section-title (format nil "Would append to ~a.~a of ~a" language type-name conjugated-word))
	       (formatted (format nil "
==~a==
~a
===Task data===
~a
" section-title template task)))
	  (api-edit test-page
		    (format nil "Conjugation table task dry run for ~a/~a" conjugated-word base-word)
		    :append formatted))))))

(defun-conjugation make-task-summary (base-word)
  (format nil
	  "böjningsform av [[~a]] (automatiserad)"
	  base-word))

(defun-conjugation edit-conjugation-task (type conjugated-word base-word grammar &key create-perfect-participles)
  (when (find :participle grammar)
    (if (and create-perfect-participles
	       (find :perfect grammar))
	(progn
	  (api-edit conjugated-word
		    (make-task-summary base-word)
		    :createonly t
		    :minor t
		    :append (swedish-new-conjugated-participle-page base-word grammar))
	  (log-info 'edit-conjugation-task
		    "created experimental participle ~a (conjugation of ~a)"
		    conjugated-word base-word))
	(log-info 'edit-conjugation-task
		  "ignoring participle ~s / ~s"
		  conjugated-word base-word))
    (return-from edit-conjugation-task nil))
  (api-edit conjugated-word
	    (make-task-summary base-word)
	    :createonly t
	    :minor t
	    :append (swedish-new-conjugated-form-page base-word type))
  (log-info 'edit-conjugation-taks
	    "created ~a (conjugation of ~a)"
	    conjugated-word base-word))

(defun-conjugation perform-collected-conjugation-tasks ()
  (simple-swedish-conjugation-tasks *collected-conjugation-tasks*)
  (setf *collected-conjugation-tasks* nil))

(#-unsafe defun-conjugation #+unsafe defun make-collect-conjugation-tasks-from-continuation (titles)
  #'(lambda (c-reschedule c-done)
      (labels ((process-task (word)
		 (log-debug 'collect-conjugation-tasks-from-continuation
			    "processing ~a"
			    word)
		 (#-unsafe with-conjugation #+unsafe progn
		   (let ((found (remove-if #'(lambda (task)
					       (find task
						     *collected-conjugation-tasks*
						     :test #'equal
						     :key #'second))
					   (swedish-missing-conjugation-links word))))
		     (log-info 'collect-conjugation-tasks-from-continuation
			       "processed ~a (found ~a new)"
			       word
			       (length found))
		     (dolist (task found)
		       (push task *collected-conjugation-tasks*)))))
	       (process-tasks (words)
		 (if (null words)
		     (funcall c-done)
		     (funcall c-reschedule
			      1
			      #'(lambda ()
				  (process-task (car words))
				  (process-tasks (cdr words)))))))
	(process-tasks titles))))

(#-unsafe defun-conjugation #+unsafe defun make-scan-dumps-for-conjugation-tasks-continuation (&key (n 1000) (delay 5))
    #'(lambda (c-reschedule c-done)
	(labels ((f ()
		   (let ((titles (swedish-pages-with-blessed-grammar-templates-in-dump
				  :sample n
				  :criterion #'(lambda (title)
						 (not (in-conjugation-check-log? title))))))
		       (if (null titles)
			   (funcall c-done)
			   (progn
			     (log-info 'scan-dumps-for-conjugation-tasks-continuation
				       "collecting conjugation tasks from ~a dump titles"
				       (length titles))
			     (funcall (make-collect-conjugation-tasks-from-continuation titles)
				      c-reschedule
				      #'(lambda ()
					  (funcall c-reschedule
						   delay
						   #'f))))))))
	  (f))))

(#-unsafe defun-conjugation #+unsafe defun make-scan-rc-for-conjugation-tasks-continuation (&key (n 50) (delay-between-scans (* 5 60)))
    #'(lambda (c-reschedule c-done)
	(declare (ignore c-done))
	(let ((last-rc-check nil))
	  (labels ((f ()
		     (let ((changed-titles (remove-duplicates (recent-changes :n n :end-timestamp last-rc-check)
							      :test #'equal)))
		       (log-info 'scan-rc-for-conjugation-tasks-continuation
				 "collecting conjugation tasks from ~a RC titles"
				 (length changed-titles))
		       (setf last-rc-check (get-universal-time))
		       (funcall (make-collect-conjugation-tasks-from-continuation changed-titles)
				c-reschedule
				#'(lambda ()
				    (funcall c-reschedule
					     delay-between-scans
					     #'f))))))
	    (f)))))

(defun-conjugation make-perform-collected-conjugation-tasks-continuation (&key (bunch-size 100) (extra-delay 1.0))
  #'(lambda (c-reschedule c-done)
      (let ((tasks (with-conjugation
		     (multiple-value-bind (now later)
			 (first-n *collected-conjugation-tasks* bunch-size)
		       (setf *collected-conjugation-tasks* later)
		       (filter-redlink-list (remove-if #'in-edit-log? now :key #'second) :key #'second)))))
	(labels ((process-task (task)
		   (destructuring-bind (type conjugated-word base-word grammar)
		       task
		     (let ((task-summary (make-task-summary base-word)))
		       (handler-case (edit-conjugation-task type conjugated-word base-word grammar)
			 (article-already-exists ()
			   (format t "late skip pre-existing ~a~%" task))))))
		 (process-tasks (tasks)
		   (if (null tasks)
		       (funcall c-done)
		       (funcall c-reschedule
				extra-delay
				#'(lambda ()
				    (process-task (car tasks))
				    (process-tasks (cdr tasks)))))))
	  (process-tasks tasks)))))
		       
	

(defun-conjugation simple-swedish-conjugation-tasks (tasks &key (extra-delay 0.5))
  (restart-case
      (dolist (task (filter-redlink-list (remove-if #'in-edit-log? tasks :key #'second) :key #'second))
	(destructuring-bind (type conjugated-word base-word grammar)
	    task
	  (let ((task-summary (make-task-summary base-word)))
	    (handler-case (edit-conjugation-task type conjugated-word base-word grammar)
	      (article-already-exists ()
		(format t "late skip pre-existing ~a~%" task)))))
	(when extra-delay
	  (sleep extra-delay)))
    (retry-conjugation-tasks ()
      :report "Retry conjugation tasks"
      (simple-swedish-conjugation-tasks tasks :extra-delay extra-delay))))    

(defun-conjugation collect-some-conjugation-tasks (&optional (n 1000))
  (collect-conjugation-tasks
   (swedish-pages-with-blessed-grammar-templates-in-dump
    :sample n
    :criterion #'(lambda (title) (not (in-conjugation-check-log? title))))))
				      