(in-package :wiktionary-bot)


(defvar *web-resource-lock* (mp:make-process-lock))
(defmacro with-web (&body body)
  `(mp:with-process-lock (*web-resource-lock*)
     ,@body))
(defmacro defun-web (name varlist &body body)
  `(defun ,name ,varlist
     (with-web ,@body)))

(defparameter *web-resource-cache-master* #p"./data/web-resources-cache-master.generated.lisp")
(defparameter *web-resource-cache-urls* #p"./data/web-resources-cache-urls.generated.lisp")
(defparameter *web-resource-cache-template* "./data/web-resources-cache/wrcache-~a.generated.lisp")

(defparameter *web-resource-cache-memory-limit* 100)
(defparameter *web-resource-quiet* nil)


(defparameter *article-fetchers* nil)

(defparameter *front-page-fetchers* nil)


(defun-web hash-table-from-pairs (pairs &key (test #'eql))
  (let ((ht (make-hash-table :test test :size (length pairs))))
    (loop :for (key . value) :in pairs :do (setf (gethash key ht) value))
    ht))

(defun-web find-sexp-in-file-satisfying (predicate filename)
  (with-open-file (*standard-input* filename
				    :direction :input
				    :if-does-not-exist nil)
    (when *standard-input*
      (handler-case (loop
		       :for x = (read)
		       :if (funcall predicate x)
		       :do (return-from find-sexp-in-file-satisfying x))
	(end-of-file () nil)))))

(defun-web list-to-file-appending (list filename)
  (with-open-file (*standard-output* filename
				     :direction :output :if-exists :append :if-does-not-exist :create)
    (dolist (element list)
      (write element)
      (write-char #\newline))))

(defun-web list-to-file-overwriting (list filename)
  (with-open-file (*standard-output* filename
				     :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (element list)
      (write element)
      (write-char #\newline))))

(defun-web list-to-file-creating (list filename)
  (with-open-file (*standard-output* filename
				     :direction :output :if-exists :error :if-does-not-exist :create)
    (dolist (element list)
      (write element)
      (write-char #\newline))))
    
(defun-web sexp-to-file-overwriting (sexp filename)
  (with-open-file (*standard-output* filename
				     :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write sexp)))

(defun-web sexp-to-file-appending (sexp filename)
  (let ((*print-pretty* nil))
    (with-open-file (*standard-output* filename
				       :direction :output :if-exists :append :if-does-not-exist :create)
      (prin1 sexp)
      (write-char #\newline)
      nil)))

(defvar *urls-cached-on-disk* (hash-table-from-pairs (list-in-file *web-resource-cache-urls*)
						     :test #'equal))

(defvar *web-resource-cache* (make-hash-table :test #'equal))


(defun-web rename-web-resource-cache-file (old-filename new-filename)
  (labels ((f (name) (if (equal name old-filename)
			 new-filename
			 name)))
    (excl.osi:rename old-filename new-filename)
    (list-to-file-overwriting (mapcar #'(lambda (entry)
					  (destructuring-bind (url . filename)
					      entry
					    (cons url (f filename))))
				      (list-in-file *web-resource-cache-urls*))
			      *web-resource-cache-urls*)
    (setf *urls-cached-on-disk* (hash-table-from-pairs (list-in-file *web-resource-cache-urls*)
						       :test #'equal))))
  
(defun-web clear-web-resource-cache ()
  (setf *web-resource-cache* (make-hash-table :test #'equal)))

(defun-web retrieve-from-disk-cache (url)
  (let ((cache-filename (gethash url *urls-cached-on-disk*)))
    (when cache-filename
      (cdr (find-sexp-in-file-satisfying #'(lambda (key-value)
					     (equal (car key-value) url))
					 cache-filename)))))

(defun-web memory-web-resources-statistics ()
  (maphash-to-unordered-list #'cons
			     (let ((ht (make-hash-table :test #'equal)))
			       (loop
				  :for url :in  (hash-table-keys *web-resource-cache*)
				  :do (let* ((resource (gethash url *web-resource-cache*))
					     (valid (web-resource-valid? resource))
					     (site (extract '(:source) resource))
					     (cons (setf (gethash site ht)
							 (or (gethash site ht nil)
							     (list 0 0)))))
					(incf (cadr cons))
					(when valid (incf (car cons)))))
			       ht)))	    

(defun-web flush-web-resource-cache-to-disk ()
  (let* ((no (or (sexp-in-file *web-resource-cache-master*) 1))
	 (filename (format nil *web-resource-cache-template* no)))
    (log-info 'flush-web-resource-cache-to-disk
	      "flushing web resources to file ~a"
	      filename)
    (dolist (entry (memory-web-resources-statistics))
      (destructuring-bind (source valid total)
	  entry
	(log-info 'flush-web-resource-cache-to-disk
		  "from ~a, ~a/~a valid"
		  source
		  valid
		  total)))
    (sexp-to-file-overwriting (1+ no) *web-resource-cache-master*)
    (dolist (url (hash-table-keys *web-resource-cache*))
      (setf (gethash url *urls-cached-on-disk*) filename))
    (list-to-file-appending (loop
			       :for url :in (hash-table-keys *web-resource-cache*)
			       :collect (cons url filename))
			    *web-resource-cache-urls*)
    (list-to-file-creating (maphash-to-unordered-list #'cons *web-resource-cache*)
			   filename)
    (clear-web-resource-cache)))
    

(defun-web web-resource-valid? (resource)
  (cond ((empty? (extract '(:text) resource))
	 (values nil "Missing text"))
	((empty? (extract '(:url) resource))
	 (values nil "Missing URL"))
	((null (extract '(:retrieved) resource))
	 (values nil "Missing retrieval timestamp"))
	((null (extract '(:title) resource))
	 (values nil "Missing title"))
	(t resource)))

(defun-web cached-web-resources ()
  (remove-if-not #'web-resource-valid?
		 (hash-table-values *web-resource-cache*)))

(defun-web fetch-cached-web-resource (url)
  (or (gethash url *web-resource-cache*)
      (retrieve-from-disk-cache url)))

(defun-web fetch-web-resource (fetcher url)
  (or (values (fetch-cached-web-resource url)
	      nil)
      (let ((result (setf (gethash url *web-resource-cache*)
			  (funcall fetcher url))))
	(unless *web-resource-quiet*
	  (log-info 'fetch-web-resource
		    "fetched ~a"
		    url))
	(when (>= (hash-table-count *web-resource-cache*)
		  *web-resource-cache-memory-limit*)
	  (flush-web-resource-cache-to-disk))
	(values result
		t))))

(defun-web filter-many (html argss)
  (dolist (args argss)
    (setf html (apply #'lhtml-filter
		      html
		      args)))
  html)    

(defun-web filter-nontextual-html (html)
  (filter-many html '((:name :ul)
		      (:name :script)
		      (:name :li))))

(let ((whitespace-regex (cl-ppcre:create-scanner "\\s+"))
      (newline-regex (cl-ppcre:create-scanner "\\n+")))
  (defun-web cleanup-whitespace-single-line (&rest strings)
    (trim (cl-ppcre:regex-replace-all whitespace-regex
				      (string-join " " strings)
				      " ")))
  (defun-web cleanup-whitespace (&rest strings)
    (remove-if (papply (equal ? ""))
	       (mapcar #'(lambda (paragraph)
			   (trim (cl-ppcre:regex-replace-all whitespace-regex paragraph " ")
				 #'(lambda (char)
				     (or (whitespace? char)
					 (eql (char-code char) 160)))))
		       (apply #'append (mapcar #'(lambda (string)
						   (cl-ppcre:split newline-regex string))
					       strings))))))

(defmacro def-media-fetcher ((front-page-function
			      article-function
			      base-url
			      &key
			      url-regex
			      use-partial-url
			      source-name
			      language
			      (include-as-standard t)
			      (url-prefix ""))
			     &key
			     title
			     author
			     text
			     published
			     updated)
  `(progn
     (defun-web ,article-function (url)
       (let* ((raw-html (parse-html (drakma-request url nil)))
	      (html (filter-nontextual-html raw-html)))
	 (append (list (cons :source ,source-name)
		       (cons :language ,language)
		       (cons :retrieved (get-universal-time))
		       (cons :url url))
		 (let ((x (cleanup-whitespace ,@text)))
		   (when (and x (not (equal "" x)))
		     (list (cons :text x))))
		 (let ((x (cleanup-whitespace-single-line ,@author)))
		   (when (and x (not (equal "" x)))
		     (list (cons :author x))))
		 (let ((x (cleanup-whitespace-single-line ,@title)))
		   (when (and x (not (equal "" x)))
		     (list (cons :title x))))
		 (let ((x (cleanup-whitespace-single-line ,@updated)))
		   (when (and x (not (equal "" x)))
		     (list (cons :updated x))))
		 (let ((x (cleanup-whitespace-single-line ,@published)))
		   (when (and x (not (equal "" x)))
		     (list (cons :published x)))))))
     (defun-web ,front-page-function ()
       (let ((url-type (cl-ppcre:create-scanner ,url-regex)))
	 (values (remove-duplicates
		  (collecting
		    (dolist (entry (lhtml-select (parse-html (drakma-request ,base-url nil))
						 :list t
						 :name :a))
		      (destructuring-bind (name attrs . contents)
			  entry
			(declare (ignore name contents))
			(let ((href (second (assoc :href attrs))))
			  (multiple-value-bind (string groups)
			      (cl-ppcre:scan-to-strings url-type href)
			    ,@(when (not use-partial-url)
				    (list (list 'declare (list 'ignore 'groups))))
			    (when string
			      (let ((good-string ,(if use-partial-url
						      (list 'aref 'groups 0)
						      'href)))
				(collect ,(if url-prefix
					      `(concatenate 'string
							    ,(if (eq url-prefix t)
								 base-url
								 url-prefix)
							    good-string)
					      'good-string)))))))))
		  :test #'equal)
		 #',article-function
		 ,language)))
     ,(if include-as-standard
	  `(progn (push #',front-page-function *front-page-fetchers*)
		  (push (cons #',article-function
			      (cl-ppcre:create-scanner ,url-regex))
			*article-fetchers*))
	  nil)))
				 
(def-media-fetcher (fetch-gp.se-front-page
		    fetch-gp.se-article
		    "http://www.gp.se"
		    :url-regex "^/(?:[a-z]+/)*?[0-9]+\\.[0-9]+-[\\w-]+$"
		    :source-name "Göteborgs-Posten"
		    :language :swedish
		    :url-prefix "http://www.gp.se")
    :text ((lhtml->text (lhtml-select html
				      :id "articlePreamble"))
	   (lhtml->text (lhtml-select html
				      :class "body")))
    :title ((third (lhtml-select html
				 :name :h1
				 :id "articleHeader")))
    :author ((lhtml->text (lhtml-select (lhtml-select html
							    :name :div
							    :class "bylineContent")
					      :name :span
					      :class "name"))))

(def-media-fetcher (fetch-svd.se-front-page
		    fetch-svd.se-article
		    "http://www.svd.se"
		    :url-regex "^http://www.svd.se/(?:[a-z]+/)+[\\w-]+_[0-9]+\\.svd$"
		    :source-name "Svenska Dagbladet"
		    :language :swedish)
    :text ((lhtml->text (lhtml-select html
				       :name :p
				       :class "preamble"))
	   (lhtml->text (lhtml-select html
				      :name :div
				      :class "articletext")))
    :author ((lhtml->text (lhtml-select html
					:name :p
					:class "author")))
    :title ((lhtml->text (lhtml-select (lhtml-select html
						     :name :div
						     ::id "article")
				       :name :h1))))

    
    
  
(defun-web fetch-from-front-pages (fetchers &key languages)
  (log-info 'fetch-from-front-pages
	    "fetching from front pages (languages ~a): ~a" languages fetchers)
  (let ((*delay-read* 5.0))
    (collecting
      (dolist (fetcher fetchers)
	(multiple-value-bind (urls article-fetcher language)
	    (funcall fetcher)
	  (let ((will-continue (or (null languages)
				   (find language languages))))
	    (log-info 'fetch-from-front-pages
		      "checked front page ~a, ~a results in ~a (continuing? ~a)"
		      fetcher
		      (length urls)
		      language
		      will-continue)
	    (when will-continue
	      (dolist (url urls)
		(collect (funcall #'fetch-web-resource article-fetcher url))))))))))

(defun rotate (list)
  (append (cdr list) (list (car list))))

(defun %mapcar-by-continuation (continue-later result-function function lists &optional acc)
  (if (null (car lists))
      (funcall result-function (reverse acc))
      (funcall continue-later
	       #'(lambda ()
		   (%mapcar-by-continuation continue-later
					    result-function
					    function
					    (mapcar #'cdr lists)
					    (cons (apply #'funcall function (mapcar #'car lists))
						  acc))))))

(defun mapcar-by-continuation (continue-later result-function function &rest lists)
  (%mapcar-by-continuation continue-later
			   result-function
			   function
			   lists))  

(defun make-fetch-from-standard-front-pages-continuation (&key (languages '(:swedish)))
  (make-fetch-from-front-pages-continuation (append *front-page-fetchers*)
					    :languages languages))

(defun make-fetch-from-front-pages-continuation (fetchers &key languages)
  #'(lambda (reschedule-continuation done-continuation)
      (log-info 'fetch-from-front-pages
		"fetching from front pages (languages ~a): ~a" languages fetchers)
	(labels ((process-url-collections (article-fetchers urlsets)
		   (if (null (car urlsets))
		       (if (null (cdr urlsets))
			   (funcall done-continuation)
			   (funcall reschedule-continuation
				    0
				    #'(lambda ()
					(process-url-collections (cdr article-fetchers)
								 (cdr urlsets)))))
		       (let ((article-fetcher (car article-fetchers))
			     (url (caar urlsets))
			     (next-article-fetchers (rotate article-fetchers))
			     (next-urlsets (append (cdr urlsets) (list (cdar urlsets)))))
			 (multiple-value-bind (resource accessed-network)
			     (fetch-web-resource article-fetcher url)
			   (declare (ignore resource))
			   (log-debug 'fetch-from-front-pages-continuation
				      "processed URL ~a"
				      url)
			   (funcall reschedule-continuation
				    (if accessed-network 5.0 0)
				    #'(lambda ()
					(process-url-collections next-article-fetchers
								 next-urlsets)))))))
		 (process-fetchers-sideways (front-page-fetchers)
		   (mapcar-by-continuation 
		    (papply (funcall reschedule-continuation 5.0 ?))
		    #'(lambda (af-urls)
			(process-url-collections (mapcar #'car af-urls)
						 (mapcar #'cdr af-urls)))
		    #'(lambda (front-page-fetcher)
			(multiple-value-bind (urls article-fetcher language)
			    (funcall front-page-fetcher)
			  (log-debug 'fetch-from-front-pages-continuation
				     "~a URLs in ~a for for ~a"
				     (length urls)
				     language
				     article-fetcher)
			  (when (or (null languages)
				    (find language languages))
			    (cons article-fetcher urls))))
		    front-page-fetchers)))
	(process-fetchers-sideways fetchers))))

(def-media-fetcher (fetch-hbl.fi-front-page
		    fetch-hbl.fi-article
		    "http://www.hbl.fi"
		    :url-regex "^/(?:[\\w]+/)+[0-9]{4}-[0-9]{2}-[0-9]{2}/(?:[\\w-]+)$"
		    :source-name "Hufvudstadsbladet"
		    :language :swedish
		    :url-prefix "http://www.hbl.fi/")
    :title ((lhtml->text (lhtml-select html
				       :name :h1
				       :id "page-title")))
    :text ((lhtml->text (lhtml-select html
				       :name :div
				       :id "nodebody")))
    :author ((lhtml->text (lhtml-select html
					:name :div
					:class "views-field-uid"))))

(defun-web string-upcase-first (string)
  (concatenate 'string
	       (string-upcase (subseq string 0 1))
	       (subseq string 1)))

(let ((whitespace (cl-ppcre:create-scanner "\\b")))
  (defun-web titlecase (string)
    (let ((words (cl-ppcre:split whitespace string)))
      (apply #'concatenate
	     'string
	     (mapcar #'string-upcase-first words)))))

(defun-web titlecase-if-all-lowercase (string)
  (if (every #'(lambda (ch) (not (upper-case-p ch)))
	     (coerce string 'list))
      (titlecase string)
      string))

(defun-web lhtml-attribute (root name)
  (destructuring-bind (tag-name attrs . contents)
      root
    (declare (ignore tag-name contents))
    (cdr (assoc name attrs))))

(defun-web empty? (string)
  (or (null string)
      (equal "" string)))

(defmacro or-string (&rest forms)
  (if (null forms)
      nil
  (let ((sym (gensym))
	(form (car forms)))
    `(let ((,sym ,form))
       (if (and ,sym (not (equal "" ,sym)))
	   ,sym
	   (or-string ,@(cdr forms)))))))

(def-media-fetcher (fetch-guardian.co.uk-front-page
		    fetch-guardian.co.uk-article
		    "http://www.guardian.co.uk"
		    :url-regex "^http://www.guardian.co.uk/(?:[a-z]+/)+[0-9]{4}/[a-z]+/[0-9]+/[\\w-]+$"
		    :source-name "The Guardian"
		    :language :british-english)
    :title ((lhtml->text (lhtml-select (lhtml-select html
						     :name :div
						     :id "main-article-info")
				       :name :h1)))
    :published ((or-string (lhtml->text (lhtml-filter (lhtml-select raw-html
								    :name :li
								    :class "publication")
						      :name :a))
			   (lhtml->text (lhtml-select raw-html
						      :name :span
						      :class "timestamp"))))
    :text ((lhtml->text (lhtml-filter (lhtml-select html
						    :name :div
						    :id "article-body-blocks")
				      :class "inline")))
    :author ((lhtml->text (lhtml-select raw-html
					:name :div
					:class "contributor-full"))))

(def-media-fetcher (fetch-aftenposten.no-front-page
		    fetch-aftenposten.no-article
		    "http://www.aftenposten.no"
		    :url-regex "^http://[a-z]+\\.aftenposten.no/(?:[\\w]+/)+[\\w-]+[0-9]+\\.html$"
		    :source-name "Aftenposten"
		    :language :norwegian)
    :title ((lhtml->text (lhtml-select html
				       :name :h1
				       :class "articleTitle")))
    
    :published
    #-html5
    ((second (cleanup-whitespace (lhtml->text (lhtml-select html
							   :class "publishInfo")))))
    #+html5
    ((lhtml-attribute (lhtml-select html
				    :name :time
				    :class "published")
		      :datetime))
    :updated
    #-html5
    ((fourth (cleanup-whitespace (lhtml->text (lhtml-select html
							   :class "publishInfo")))))
    #+html5
    ((lhtml-attribute (lhtml-select html
				    :name :time
				    :class "updated")
		      :datetime))
    :text ((lhtml->text (lhtml-select html
				      :name :p
				      :class "leadText"))
	   (lhtml->text (filter-many (lhtml-select html
						   :name :div
						   :class "storyContent")
				     '((:name :h2)
				       (:name :h3)
				       (:name :h4)
				       (:name :h5)
				       (:name :h6)))))
    :author ((cl-ppcre:regex-replace-all "\\s+"
					 (titlecase (let ((s (lhtml->text (lhtml-select (lhtml-select html
												      :name :p
												      :class "author")
											:name :a
											:class "email"))))
						      (if (not (equal "" s))
							  s
							  (lhtml->text (lhtml-filter (lhtml-select html
												   :name :p
												   :class "author")
										     :class "prefix")))))
					 " ")))

(defun-web fetch-from-standard-front-pages (&key (languages '(:swedish)))
  (fetch-from-front-pages (append *front-page-fetchers*
				  (list #'fetch-gp.se-front-page
					#'fetch-svd.se-front-page))
			  :languages languages))

(defun-web indexed-sentence-words (sentence)
  (mapcar #'second (third sentence)))

(defun-web article-indexed-sentences (text)
  (when (consp text)
    (return-from article-indexed-sentences
      (apply #'append
	     (mapcar #'article-indexed-sentences
		     text))))
  (labels ((chunk (b-e) (destructuring-bind (begin . end)
			    b-e
			  (subseq text begin end))))
    (loop
     :for begin-ends :in  (tokens->sentences (tokenize text :offsets t)
					     :key #'chunk)
     :collect (list (caar begin-ends)
		    (cdar (last begin-ends))
		    (mapcar #'(lambda (b-e)
				(list (car b-e) (chunk b-e)))
			    begin-ends)))))

(defun-web web-resource-indexed-sentences (web-resource)
  (article-indexed-sentences (extract '(:text) web-resource)))

(defun web-resource-unindexed-sentences (web-resource)
  (mapcar #'indexed-sentence-words (web-resource-indexed-sentences web-resource)))

(defun flatten (sequence-of-sequences)
  (reduce #'append
	  sequence-of-sequences
	  :from-end t))  

(defun-web highlight-in (text begin end &optional (begin-marker "**") end-marker)
  (let ((end-marker (or end-marker begin-marker)))
    (concatenate 'string
		 (subseq text 0 begin)
		 begin-marker
		 (subseq text begin end)
		 end-marker
		 (subseq text end))))

(defun-web web-resource-unknown-words (resource)
  (cons (cons :unknown (article-unknown-words (cdr (assoc :text resource))))
	resource))
	  

(defun-web article-unknown-words (text)
  (collecting
    (dolist (sentence (article-indexed-sentences text))
      (destructuring-bind (sentence-begin sentence-end indexed-words)
	  sentence
	(dolist (indexed-word (scan-sentence-for-unknown-words indexed-words :key #'second))
	  (destructuring-bind (word-begin word)
	      indexed-word
	    (collect (list word
			   (subseq text sentence-begin sentence-end)
			   (- word-begin sentence-begin)
			   (+ (length word) (- word-begin sentence-begin))))))))))

(defun-web collect-media-loop (&key (languages '(:swedish)) (interval (* 60 30)))
  (loop :do (progn
	      (fetch-from-standard-front-pages :languages languages)
	      (log-info 'collect-media-loop "sleeping for ~a seconds" interval)
	      (sleep interval))))

(defun-web all-web-resources ()
  (append (hash-table-values *web-resource-cache*)
	  (maphash-to-unordered-list
	   #'(lambda (url disk-file)
	       (declare (ignore disk-file))
	       (retrieve-from-disk-cache url))
	   *urls-cached-on-disk*)))

(defun-web on-all-web-resources-continuation (c-next
					      c-finished
					      function)
  (let ((in-memory (maphash-to-unordered-list #'cons *web-resource-cache*))
	(on-disk (hash-table-keys *urls-cached-on-disk*))
	(results))
    (labels ((process-next-in-memory (rest-of-memory)
	       (funcall c-next
			#'(lambda ()
			    (if (null rest-of-memory)
				(process-next-on-disk on-disk)
				(progn
				  (push (funcall function (cdar rest-of-memory)) results)
				  (process-next-in-memory (cdr rest-of-memory)))))))
	     (process-next-on-disk (rest-of-disk)
	       (if (null rest-of-disk)
		   (funcall c-finished results)
		   (funcall c-next #'(lambda ()
				       (push (funcall function
						      (retrieve-from-disk-cache (car rest-of-disk)))
					     results)
				       (process-next-on-disk (cdr rest-of-disk)))))))
      (funcall c-next
	       #'(lambda ()
		   (process-next-in-memory in-memory))))))

(defun on-all-web-resources (function)
  (let ((result-value)
	(result-p))
    (loop
       :until result-p
       :do (on-all-web-resources-continuation #'(lambda (future)
						  (schedule future 0))
					      #'(lambda (result)
						  (setf result-p t
							result-value result))
					      function)
       :do (sleep 0.5))
    result-value))

(defun select-substring (string regex)
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings regex string)
    (when match
      (aref groups 0))))

(defun debug-show-element (element &optional description)
  (log-debug 'debug-show-element
	     "~a: ~a"
	     (or description "element")
	     element)
  element)

(defun one-matching (sequence predicate &key key)
  (let ((rv (remove-if-not predicate sequence :key key)))
    (if (eql (length rv) 1)
	(car rv)
	nil)))

(defun first-matching (sequence predicate &key key)
  (car (remove-if-not predicate sequence :key key)))

(defun text-contains-predicate (text)
  #'(lambda (element)
      (search text (lhtml->text element))))

(defun one-containing-text (sequence text)
  (one-matching sequence (text-contains-predicate text)))

(defun first-containing-text (sequence text)
  (first-matching sequence (text-contains-predicate text)))

(def-media-fetcher (fetch-sydsvenskan.se-front-page
		    fetch-sydsvenskan.se-article
		    "http://www.sydsvenskan.se"
		    :url-regex "^/[a-z\\-]+/[a-z\\-]+$"
		    :url-prefix t
		    :language :swedish
		    :source-name "Sydsvenska Dagbladet Snällposten")
    :title ((call-sequence html
			   (lhtml-select :name :h1 :class "saplo:headline")
			   (lhtml->text)))
    :text ((call-sequence html
			   (lhtml-select :name :div :class "saplo:lead")
			   (lhtml->text))
	    (call-sequence html
			   (lhtml-select :name :div :class "saplo:body")
			   (lhtml->text)))
    :author ((call-sequence html
			    (lhtml-select :name :div :class "two_column_right")
			    (lhtml-select-list :name :p :class "toolbar")
			    (one-containing-text "Författare")
			    (lhtml-select :name :strong)
			    (lhtml->text)))
    :published ((call-sequence html
			    (lhtml-select :name :div :class "two_column_right")
			    (lhtml-select-list :name :p :class "toolbar")
			    (one-containing-text "Författare")
			    (lhtml-select-list :name :span :class "date")
			    (one-containing-text "Publicerad")
			    (lhtml->text)
			    (select-substring "Publicerad ([0-9]+ \\w+ [0-9]+ [0-9]{2}\\.[0-9]{2})")))
    :updated ((call-sequence html
			    (lhtml-select :name :div :class "two_column_right")
			    (lhtml-select-list :name :p :class "toolbar")
			    (one-containing-text "Författare")
			    (lhtml-select-list :name :span :class "date")
			    (one-containing-text "Uppdaterad")
			    (lhtml->text)
			    (select-substring "Uppdaterad ([0-9]+ \\w+ [0-9]+ [0-9]{2}\\.[0-9]{2})"))))

(def-media-fetcher (fetch-di.se-front-page
		    fetch-di.se-article
		    "http://di.se/Nyheter/"
		    :url-regex "^(/\\w+/[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}/[0-9]+/[\\w-]+/)\\?.*$"
		    :source-name "Dagens Industri"
		    :use-partial-url t
		    :language :swedish
		    :url-prefix "http://di.se")
    :author ((call-sequence html
			    (lhtml-select :name :div :class "content")
			    (lhtml-select :name :div :class "sign")
			    (lhtml-select :name :a :href '(regex "^mailto:"))
			    (lhtml->text)))
    :text ((call-sequence html
			  (lhtml-select :name :div :id "articleIntro")
			  (lhtml-filter :class "panel-float-container")
			  (lhtml->text))
	   (call-sequence html
			  (lhtml-select :name :div :id "articleBody")
			  (lhtml-filter :class "panel-float-container")
			  (lhtml->text)))
    :title ((call-sequence html
			   (lhtml-select :name :div :id "phArticle")
			   (lhtml-select :name :h1)
			   (lhtml->text))))

(def-media-fetcher (fetch-dn.se-front-page
		    fetch-dn.se-article
		    "http://www.dn.se"
		    :source-name "Dagens Nyheter"
		    :url-regex "^(?:/[a-z\\-]+/)+-?-?[a-z]+-[a-z\\-]+$"
		    :url-prefix t
		    :language :swedish)
    :title ((call-sequence html
			   (lhtml-select :name :div :id "article-content")
			   (lhtml-select :name :h1)
			   (lhtml->text)))
    :text ((call-sequence html
			  (lhtml-select :name :div :id "article-content")
			  (lhtml-select :name :div :class "preamble")
			  (lhtml->text))
	   (call-sequence html
			  (lhtml-filter :name :div :class "advert-space")
			  (lhtml-select :name :div :id "article-content")
			  (lhtml-select :name :div :id "contentBody")
			  (lhtml->text)))
    :author ((call-sequence html
			    (lhtml-select :name :div :id "article-content")
			    (lhtml-select :name :div :class "byline")
			    (lhtml-select :name :strong)
			    (lhtml->text))))
    
(def-media-fetcher (fetch-dagen.se-front-page
		    fetch-dagen.se-article
		    "http://www.dagen.se/Nyheter/"
		    :url-regex "^http://www.dagen.se/\\w+/[a-z\\-]+/$"
		    :source-name "Dagen"
		    :language :swedish)
    :author ((call-sequence html
			    (lhtml-select :name :div :class "articleByline")
			    (lhtml-select :name :p :class "fullname")
			    (lhtml->text)))
    :published ((call-sequence html
			    (lhtml-select :name :div :class "articleByline")
			    (lhtml-select :name :div :class "date")
			    (lhtml-select :name :p :class "published")
			    (lhtml->text)
			    (select-substring "Publicerat:\\s+([0-9\\-]+)")))
    :updated ((call-sequence html
			     (lhtml-select :name :div :class "articleByline")
			     (lhtml-select :name :div :class "date")
			     (lhtml-select :name :p :class "updated")
			     (lhtml->text)
			     (select-substring "Uppdaterat:\\s+([0-9\\-]+)")))
    :title ((call-sequence html
			   (lhtml-select :name :div :class "article")
			   (lhtml-select :name :header)
			   (lhtml-select :name :h1)
			   (lhtml->text)))
    :text ((call-sequence html
			  (lhtml-select :name :div :class "article")
			  (lhtml-select :name :header)
			  (lhtml-select :name :p :class "leading")
			  (lhtml->text))
	   (call-sequence html
			  (lhtml-select :name :div :class "articleText")
			  (lhtml->text))))
			   



