(in-package :wiktionary-bot)

(defstruct indexed-dump stream map)

(defparameter *raw-swedish-dump-filename* #p"./data/svwiktionary-20120516-pages-meta-current.xml")
(defparameter *swedish-data-filename* #p"./data/swedish-dump.data")
(defparameter *swedish-index-filename* #p"./data/swedish-dump.index")
(defvar *swedish-indexed-dump* nil)

(defun parse-element (source element-name &key ignore-characters (make-last-read t))
  (collecting
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
				 (parse-element source v))))
		 ((eq event :comment)
		  (klacks:peek-next source))
		 (t
		  (error (format nil "was not expecting: ~a ~a ~a ~a" event u v w)))))))))

(defun parse-page (source &key (make-last-read t))
  (loop
     :until (not (eq :characters (klacks:peek source)))
     :do (klacks:peek-next source))
  (parse-element source "page" :ignore-characters t :make-last-read make-last-read))

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
		   (write (cons (cadr (assoc :title page)) current-position)
			  :stream index-file)
		   (write-char #\newline index-file)
		   (incf pages-processed)
		   (when (zerop (mod pages-processed 1000))
		     (format t
			     "[index-dump] indexing ~a into ~a/~a, ~a pages processed~%"
			     pathname
			     index-filename
			     parsed-filename
			     pages-processed)))))))))

(defun open-indexed-dump (data-file-name index-file-name)
  (make-indexed-dump :stream
		     (open data-file-name :direction :input)
		     :map
		     (let ((ht (make-hash-table :test #'equal)))
		       (with-open-file (f index-file-name :direction :input)
			 (loop
			    :for x = (read f nil nil)
			    :until (null x)
			    :do (progn
				  (setf (gethash (car x) ht) (cdr x)))))
		       ht)))

(defun close-indexed-dump (dump)
  (close (indexed-dump-stream dump))
  (setf (indexed-dump-stream dump) nil)
  (setf (indexed-dump-map dump) nil))

(defun indexed-dump-lookup (dump title)
  (let ((position (gethash title (indexed-dump-map dump)))
	(stream (indexed-dump-stream dump)))
    (file-position stream position)
    (read stream)))

(defun indexed-dump-titles (dump)
  (hash-table-keys (indexed-dump-map dump)))

(defun swedish-indexed-dump ()
  (or *swedish-indexed-dump*
      (setf *swedish-indexed-dump*
	    (open-indexed-dump *swedish-data-filename* *swedish-index-filename*))))

(defun close-swedish-indexed-dump ()
  (close-indexed-dump *swedish-indexed-dump*)
  (setf *swedish-indexed-dump* nil))

(defun swedish-dump-lookup (title)
  (indexed-dump-lookup (swedish-indexed-dump) title))