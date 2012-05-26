(in-package :wiktionary-bot)

(let ((regex (cl-ppcre:create-scanner "{{böjning\\|sv\\|([^|}]+)\\|(text=)?[^}]+}}")))
  (defun word-class-in-swedish-conjugation-link (source)
    (multiple-value-bind (match groups)
	(cl-ppcre:scan-to-strings regex source)
      (when match
	(return-from word-class-in-swedish-conjugation-link (aref groups 0))))))

(defun print-swedish-word-classes ()
  (let ((results nil))
    (dolist (title (swedish-dump-titles :namespace "0"))
      (let ((result (word-class-in-swedish-conjugation-link (swedish-dump-text title))))
	(when (and result
		   (not (find result results :test #'equal)))
	  (push result results)
	  (format t "~a~%" result))))
    results))

(defun count-swedish-word-classes ()
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

(defun swedish-conjugation-pos (type)
  (cond ((find type '(:noun :subst :substantiv))
	 "subst")
	((find type '(:adjective :adj :adjektiv))
	 "adj")
	((find type '(:verb))
	 "verb")
	(t (error (format nil "unrecognized Swedish conjugation POS: ~a" type)))))

(defun create-swedish-conjugation-link (target type)
  (concatenate 'string
	       "{{böjning|sv|"
	       (swedish-conjugation-pos type)
	       "|"
	       target
	       "}}"))

(defun swedish-pages-with-blessed-grammar-templates-in-dump ()
  (let ((regexes (mapcar #'car (swedish-grammar-table-regexes))))
    (loop
       :for title :in (swedish-dump-titles :namespace "0")
       :if (let ((source (swedish-dump-text title)))
	     (some #'(lambda (regex)
		       (cl-ppcre:scan regex source))
		   regexes))
       :collect title)))

(defun create-swedish-conjugation-link-pattern (target type)
  (cl-ppcre:create-scanner (concatenate 'string
					"{{böjning\\|sv\\|"
					(swedish-conjugation-pos type)
					"\\|"
					target ;; obs
					"}}")))

(defun parse-swedish-grammar-table (table)
  (let ((functions (list (cons #'parse-swedish-noun-table :noun)
			 (cons #'parse-swedish-verb-table :verb)
			 (cons #'parse-swedish-adjective-noncomparative-table :adjective)
			 (cons #'parse-swedish-adjective-table :adjective))))
    (destructuring-dolist ((function . type) functions)
      (let ((result (funcall function table)))
	(when result
	  (return-from parse-swedish-grammar-table
	    (list result type)))))))

(let ((robotskapad-regex (cl-ppcre:create-scanner "{{robotskapad}}")))
  (defun source-has-taboo? (source)
    (or (cl-ppcre:scan robotskapad-regex source))))

(defun swedish-missing-conjugation-links (word)
  (collecting
    (destructuring-dolist ((conjugations pos)
			   (remove-if-not #'identity
					  (mapcar #'parse-swedish-grammar-table
						  (scan-for-grammar-tables (swedish-grammar-table-regexes)
									   word))))
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
	    (let ((pattern (create-swedish-conjugation-link-pattern word pos)))
	      (unless (and (swedish-dump-text conjugated-word)
			   (cl-ppcre:scan pattern (swedish-dump-text conjugated-word)))
		(unless (or (source-has-taboo? (swedish-dump-text word))
			    (source-has-taboo? (swedish-dump-text conjugated-word)))
		  (collect (list pos conjugated-word word conjugation)))))))))))

(defvar *collected-conjugation-tasks* nil)

(defun collect-conjugation-tasks (words)
  (loop
     :for word :in words
     :for number :from 1
     :do (loop
	    :for task :in (swedish-missing-conjugation-links word)
	    :do (push task *collected-conjugation-tasks*))
     :do (format t "~a [collect-conjugation-tasks] processed #~a of ~a: ~a~%"
		 (rfc3339:make-timestamp)
		 number
		 (length words)
		 word)))

(defun swedish-new-conjugated-form-page (base-form type)
  (format nil "==Svenska==
===~a===
'''{{subst:PAGENAME}}'''
#~a

<!--[[-->" (swedish-long-pos-name type) (create-swedish-conjugation-link base-form type)))

(defun swedish-long-pos-name (type)
  (case type
    (:adjective "Adjektiv")
    (:noun "Substantiv")
    (:verb "Verb")))

(defun simulate-swedish-conjugation-task (task &key (test-page "User:MetallmanulBot/Grammar"))
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

(defun simple-swedish-conjugation-task (task &key (extra-delay 10))
  (destructuring-bind (type conjugated-word base-word grammar)
      task
    (cond ((page-source conjugated-word)
	   (format t "skipping ~a, page already exists" task))
	  (t
	   (let ((task-summary (format nil
				       "böjningsform av [[~a]] (automatiserad)"
				       base-word)))
	     (api-edit conjugated-word
		       task-summary
		       :createonly t
		       :minor t
		       :append (swedish-new-conjugated-form-page base-word type)))))
    (when extra-delay
      (sleep extra-delay))))