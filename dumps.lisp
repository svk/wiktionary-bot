(in-package :wiktionary-bot)

(defstruct indexed-dump stream map)

(defun parse-element (source element-name &key ignore-characters (make-last-read t) clean-characters)
  (let ((rv (collecting
	      (klacks:skip source :start-element nil element-name element-name)
	      (let ((quit-this))
		(loop
		   :until quit-this
		   :do
		   (multiple-value-bind (event u v w)
		       (klacks:peek source)
		     (cond ((eq event :characters)
			    (unless ignore-characters
			      (collect u))
			    (klacks:peek-next source))
			   ((eq event :end-element)
			    (when make-last-read
			      (klacks:peek-next source))
			    (setf quit-this t))
			   ((eq event :start-element)
			    (collect (cons (intern (string-upcase v) :keyword)
					   (merge-adjacent-strings
					    (parse-element source v :clean-characters clean-characters)))))
			   ((eq event :comment)
			    (klacks:peek-next source))
			   (t
			    (error (format nil "was not expecting: ~a ~a ~a ~a" event u v w))))))))))
    (if (and clean-characters
	     (some (compose #'not #'stringp)
		   rv))
	(remove-if #'stringp rv)
	rv)))

(defun parse-page (source &key (make-last-read t))
  (loop
     :until (not (eq :characters (klacks:peek source)))
     :do (klacks:peek-next source))
  (parse-element source "page" :ignore-characters t :make-last-read make-last-read :clean-characters t))

(defun index-dump (pathname parsed-filename index-filename)
  (with-open-file (parsed-file parsed-filename :direction :output :if-exists :error)
    (with-open-file (index-file index-filename :direction :output :if-exists :error)
      (klacks:with-open-source (source (cxml:make-source pathname))
	(let ((pages-processed 0))
	  (klacks:find-element source "page")
	  (loop
	     :for count :from 1
	     :do (loop
		    :until (not (eq :characters (klacks:peek source)))
		    :do (klacks:peek-next source))
	     :until (or (null (klacks:peek source))
			(eq (klacks:peek source) :end-element)
			(eq (klacks:peek source) :end-document))
	     :do (let ((page (parse-page source))
		       (current-position (file-position parsed-file)))
		   (write page :stream parsed-file)
		   (write-char #\newline parsed-file)
		   (write (list (cadr (assoc :title page))
				current-position
				(cadr (assoc :ns page)))
			  :stream index-file)
		   (write-char #\newline index-file)
		   (incf pages-processed)
		   (when (zerop (mod pages-processed 1000))
		     (format t
			     "[index-dump] ~a pages processed, indexing ~a into ~a/~a~%"
			     pages-processed
			     pathname
			     index-filename
			     parsed-filename)))))))))

(defun open-indexed-dump (data-file-name index-file-name &key (sanitize t))
  (make-indexed-dump :stream
		     (open data-file-name :direction :input)
		     :map
		     (let ((ht (make-hash-table :test #'equal)))
		       (with-open-file (f index-file-name :direction :input)
			 (loop
			    :for x = (read f nil nil)
			    :for i :from 1
			    :until (null x)
			    :do (destructuring-bind (title position namespace)
				    (if sanitize
					(sanitize-all-strings x)
					x)
				  (setf (gethash title ht)
					(list position namespace)))
			    :do (when (zerop (mod i 1000))
				  (format t "[open-indexed-dump] ~a pages processed~%" i))))
		       ht)))

(defun close-indexed-dump (dump)
  (close (indexed-dump-stream dump))
  (setf (indexed-dump-stream dump) nil)
  (setf (indexed-dump-map dump) nil))

(defun indexed-dump-lookup (dump title &key (sanitize t))
  (let ((position (car (gethash title (indexed-dump-map dump))))
	(stream (indexed-dump-stream dump)))
    (when position
      (file-position stream position)
      (let ((*read-eval* nil))
	(let ((result (read stream)))
	  (if sanitize
	      (sanitize-all-strings result)
	      result))))))

(defun indexed-dump-titles (dump &key namespace)
  (collecting
    (maphash #'(lambda (title value)
		 (destructuring-bind (position actual-namespace)
		     value
		   (declare (ignore position))
		   (when (or (null namespace)
			     (equal namespace actual-namespace))
		     (collect title))))
	     (indexed-dump-map dump))))

(defmacro def-indexed-dump (var data-filename index-filename raw-filename get close)
  `(progn
     (defvar ,var nil)
     (defun ,get ()
       (or ,var
	   (setf ,var
		 (restart-case
		     (open-indexed-dump ,data-filename ,index-filename)
		   (generate-index-dump ()
		     :report ,(format nil "Generate the indexed dump from ~a" raw-filename)
		     (index-dump ,raw-filename ,data-filename ,index-filename)
		     (open-indexed-dump ,data-filename ,index-filename))))))
     (defun ,close ()
       (close-indexed-dump ,var)
       (setf ,var nil))))

(def-indexed-dump *swedish-indexed-dump*
    #p"./data/swedish-dump.data"
    #p"./data/swedish-dump.index"
    #p"./data/svwiktionary-20120516-pages-meta-current.xml"
    swedish-indexed-dump
    close-swedish-indexed-dump)

(def-indexed-dump *swedish-indexed-wp-dump*
    #p"./data/swedish-wp-dump.data"
    #p"./data/swedish-wp-dump.index"
    #p"./data/svwiki-20120514-pages-meta-current.xml"
    swedish-indexed-wp-dump
    close-swedish-indexed-wp-dump)
    
#+fdjsk
(defun swedish-indexed-dump ()
  (or *swedish-indexed-dump*
      (setf *swedish-indexed-dump*
	    (restart-case
		(open-indexed-dump *swedish-data-filename* *swedish-index-filename*)
	      (generate-index-dump ()
		:report "Generate the index dump files"
		(index-dump *raw-swedish-dump-filename* *swedish-data-filename* *swedish-index-filename*)
		(open-indexed-dump *swedish-data-filename* *swedish-index-filename*))))))

#+dfjsk
(defun close-swedish-indexed-dump ()
  (close-indexed-dump *swedish-indexed-dump*)
  (setf *swedish-indexed-dump* nil))

(defun swedish-dump-lookup (title)
  (indexed-dump-lookup (swedish-indexed-dump) title))

(defun dump-entry-text (entry)
  (extract '(:revision :text 0) entry))

(defun swedish-dump-titles (&key namespace)
  (indexed-dump-titles (swedish-indexed-dump) :namespace namespace))

(defun swedish-dump-text (title)
  (dump-entry-text (swedish-dump-lookup title)))

(let ((regex (cl-ppcre:create-scanner "([^:]+):.+")))
  (defun title-category (title)
    (multiple-value-bind (match groups)
	(cl-ppcre:scan-to-strings regex title)
      (and match
	   (aref groups 0)))))

(defun title-categories (titles)
  (let ((categories))
    (loop :for title :in titles :do (pushnew (title-category title) categories :test #'equal))
    categories))

(let ((surrogate-list (list (cl-unicode:property-symbol "Block:High Private Use Surrogates")
			    (cl-unicode:property-symbol "Block:High Surrogates")
			    (cl-unicode:property-symbol "Block:Low Surrogates"))))
  (defun sanitize-character (character)
    (cond ((find character surrogate-list :test #'cl-unicode:has-property)
	       #\?)
	      (t character))))

(defun sanitize-string (string)
  (map 'string #'sanitize-character string))

(defun sanitize-all-strings (structure)
  (cond ((consp structure)
	 (cons (sanitize-all-strings (car structure))
	       (sanitize-all-strings (cdr structure))))
	((stringp structure) (sanitize-string structure))
	(t structure)))


(defun swedish-wp-titles-sample (&optional (n 1000))
  (single-value (first-n (indexed-dump-titles (swedish-indexed-wp-dump) :namespace "0") n)))

(defun swedish-wp-titles-random-sample (&optional (n 1000))
  (single-value (first-n (randomize-list (indexed-dump-titles (swedish-indexed-wp-dump) :namespace "0")) n)))

(defun swedish-wp-text (title)
  (dump-entry-text (indexed-dump-lookup (swedish-indexed-wp-dump) title)))

(def-simple-cached swedish-title-table
  (let ((ht (make-hash-table :test #'equal)))
    (loop
       :for title :in  (swedish-dump-titles :namespace "0")
       :do (setf (gethash (string-upcase title) ht) title))
    ht))
