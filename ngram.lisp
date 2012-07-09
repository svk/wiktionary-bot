(in-package :wiktionary-bot)

(defun standard-symbolize (thing)
  (intern (if (symbolp thing)
	      thing
	      (string-upcase thing))
	  :keyword))


(defstruct token-preprocessing
  function
  symbolize)

(let ((regex (cl-ppcre:create-scanner "^[0-9][,. 0-9]*$")))
  (defun preprocess-exclude-numbers (word)
    (cond ((cl-ppcre:scan regex word) :NUMBER)
	  (t word))))

(defun preprocess-swedish-wiktionary-pos (word)
  (let ((poses (swedish-wiktionary-pos word)))
    (cond ((null poses) :POS-UNKNOWN)
	  ((null (cdr poses)) (car poses))
	  (t :POS-MULTIPLE))))

(defun create-token-preprocessing (function &key (symbolize #'standard-symbolize))
  (make-token-preprocessing :function function
			    :symbolize symbolize))

(defun preprocess-token (scheme word)
  (funcall (token-preprocessing-symbolize scheme)
	   (funcall (token-preprocessing-function scheme) word)))

(let ((wordpp (create-token-preprocessing #'preprocess-exclude-numbers)))
  (defun preprocess-word (word) (preprocess-token wordpp word)))

(let ((pospp (create-token-preprocessing #'preprocess-swedish-wiktionary-pos)))
  (defun preprocess-pos (word) (preprocess-token pospp word)))

(defstruct ngram-collection
  preprocessing-function
  preprocessing-function-name
  ns
  data
  ngram-filter
  ngram-filter-name
  resources-consumed)

(defun write-ngram-collection (collection &optional (stream *standard-output*))
  (let ((*standard-output* stream))
    (print (cons :ns (ngram-collection-ns collection)))
    (print (cons :preprocessing-function-name (ngram-collection-preprocessing-function-name collection)))
    (print (cons :ngram-filter-name (ngram-collection-ngram-filter-name collection)))
    (print (cons :resources-consumed (maphash-to-unordered-list #'cons (ngram-collection-resources-consumed collection))))
    (print (cons :entries (loop :for n :in (ngram-collection-ns collection) :nconc (ngram-entries collection n))))
    nil))

(defun ngram-collection-to-file-overwriting (collection filename)
  (with-open-file (f filename :direction :output :if-exists :supersede)
    (write-ngram-collection collection f)))

(defun ngram-collection-from-file (filename)
  (let* ((alist (list-in-file filename))
	 (ns (cdr (assoc :ns alist)))
	 (preprocessing-function-name (cdr (assoc :preprocessing-function-name alist)))
	 (ngram-filter-name (cdr (assoc :ngram-filter-name alist)))
	 (resources-consumed (cdr (assoc :resources-consumed alist)))
	 (entries (cdr (assoc :entries alist)))
	 (c (create-ngram-collection preprocessing-function-name :ns ns :ngram-filter-name ngram-filter-name)))
    (dolist (resource resources-consumed)
      (setf (gethash resource (ngram-collection-resources-consumed c)) t))
    (dolist (entry entries)
      (increment-ngram c (car entry) (cdr entry)))
    c))

(defun create-ngram-collection (preprocessing-function-name &key ns n min-n max-n ngram-filter-name)
  (let ((ns (or ns
		(when n
		  (list n))
		(when (and min-n max-n)
		  (loop :for n :from min-n :to max-n :collect n)))))
    (assert ns)
    (make-ngram-collection :preprocessing-function (when preprocessing-function-name
						     (symbol-function preprocessing-function-name))
			   :preprocessing-function-name preprocessing-function-name
			   :ngram-filter (when ngram-filter-name
					   (symbol-function ngram-filter-name))
			   :ngram-filter-name ngram-filter-name
			   :resources-consumed (make-hash-table :test #'equal)
			   :ns ns
			   :data (make-hash-table :test #'eql))))

(defun %increment-ngram (data delta index &rest indices)
  (if (null indices)
      (incf (gethash index data 0) delta)
      (apply #'%increment-ngram
	     (or (gethash index data)
		 (setf (gethash index data)
		       (make-hash-table :test #'eq)))
	     delta
	     indices)))

(defun increment-ngram (collection ngram &optional (delta 1))
  (let ((cell (gethash (length ngram) (ngram-collection-data collection))))
    (when (null cell)
      (setf cell
	    (setf (gethash (length ngram) (ngram-collection-data collection))
		  (cons 0 (make-hash-table :test #'eq)))))
    (incf (car cell) delta)
    (apply #'%increment-ngram
	   (cdr cell)
	   delta
	   ngram)))

(defun include-ngram? (collection ngram)
  (and (some #'identity ngram)
       (or (null (ngram-collection-ngram-filter collection))
	   (funcall (ngram-collection-ngram-filter collection) ngram))))

(defun process-ngram (collection ngram &optional (multiplier 1))
  (if (include-ngram? collection ngram)
      (increment-ngram collection ngram multiplier)
      0))

(defun strict-pos-filter (ngram)
  (not (find-if #'(lambda (pos)
		    (or (eq pos :pos-unknown)
			(eq pos :pos-multiple)))
		ngram)))

(defun process-sentence (collection sentence &optional (weight 1))
  (do* ((max-n (reduce #'max (ngram-collection-ns collection)))
	(min-n (reduce #'min (ngram-collection-ns collection)))
	 (s (nconc (loop :for i :from 1 :below max-n :collect nil)
		  (mapcar (ngram-collection-preprocessing-function collection) sentence)
		  (loop :for i :from 1 :below max-n :collect nil))
	   (cdr s))
	(l (length s) (1- l)))
       ((< l min-n))
    (loop
       :for n :in (ngram-collection-ns collection)
       :if (>= l n)
       :do (process-ngram collection (first-n s n) weight)))) ;; kitty

(defvar *word-ngrams* (create-ngram-collection 'preprocess-word :min-n 1 :max-n 2))
(defvar *pos-ngrams* (create-ngram-collection 'preprocess-pos :min-n 1 :max-n 3 :ngram-filter-name 'strict-pos-filter))

(defun reset-ngram-collections ()
  (setf *word-ngrams* (create-ngram-collection 'preprocess-word :min-n 1 :max-n 2))
  (setf *pos-ngrams* (create-ngram-collection 'preprocess-pos :min-n 1 :max-n 3 :ngram-filter-name 'strict-pos-filter)))

(defun process-web-resource (collection web-resource &key reprocess)
  (let ((id (concatenate 'string
			 "web-resource:"
			 (extract '(:url) web-resource))))
    (unless (and (not reprocess)
		 (gethash id (ngram-collection-resources-consumed collection)))
      (setf (gethash id (ngram-collection-resources-consumed collection)) t)
      (dolist (sentence (web-resource-unindexed-sentences web-resource))
	(process-sentence collection sentence)))))

(defun on-ngram-entries (collection n function)
  (let ((entries (cdr (gethash n (ngram-collection-data collection)))))
    (labels ((f (entry n previous-tokens)
	       (if (zerop n)
		   (funcall function (reverse previous-tokens) entry)
		   (maphash #'(lambda (subtoken subentry)
				(f subentry (1- n) (cons subtoken previous-tokens)))
			    entry))))
      (maphash #'(lambda (token entry)
		   (f entry (1- n) (list token)))
	       entries))))

(defun show-entry (ngram count)
  (format t "~a: ~a~%" ngram count))

(defun ngram-entries (collection n)
  (collecting
    (on-ngram-entries collection n #'(lambda (ngram count) (collect (cons ngram count))))))
    
  