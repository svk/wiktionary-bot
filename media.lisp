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

(defparameter *web-resource-cache-memory-limit* 500)
(defparameter *web-resource-quiet* nil)

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

(defun-web clear-web-resource-cache ()
  (setf *web-resource-cache* (make-hash-table :test #'equal)))

(defun-web retrieve-from-disk-cache (url)
  (let ((cache-filename (gethash url *urls-cached-on-disk*)))
    (when cache-filename
      (find-sexp-in-file-satisfying #'(lambda (key-value)
					(equal (car key-value) url))
				    cache-filename))))

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

(defun-web fetch-gp.se-article (url)
  (let ((html (parse-html (drakma-request url nil))))
    (list
     (cons :title (trim (third (lhtml-select html
					     :name :h1
					     :id "articleHeader"))))
     (cons :source "Göteborgs-Posten")
     (cons :author (lhtml->text (lhtml-select (lhtml-select html
							    :name :div
							    :class "bylineContent")
					      :name :span
					      :class "name")))
     (cons :language :swedish)
     (cons :retrieved (get-universal-time))
     (cons :url url)
     (cons :text (trim (concatenate 'string
				    (lhtml->text (lhtml-select html
							       :id "articlePreamble"))
				    " "
				    (lhtml->text (lhtml-select html
							       :class "body"))))))))

(defun-web fetch-gp.se-front-page ()
  (let ((url-type (cl-ppcre:create-scanner "^/(?:[a-z]+/)*?[0-9]+\\.[0-9]+-[\\w-]+$")))
    (values (remove-duplicates
	     (collecting
	       (dolist (entry (lhtml-select (parse-html (drakma-request "http://www.gp.se" nil))
					    :list t
					    :name :A))
		 (destructuring-bind (name attrs . contents)
		     entry
		   (declare (ignore contents name))
		   (let ((href (second (assoc :href attrs))))
		     (when (cl-ppcre:scan url-type href)
		       (collect (concatenate 'string
					     "http://www.gp.se"
					     href)))))))
	     :test #'equal)
	    #'fetch-gp.se-article
	    :swedish)))

(defun-web lhtml-filter (root &key name id class)
  (if (or (null root)
	  (stringp root))
      root
      (destructuring-bind (tag-name attrs . contents)
	  root
	(list* tag-name
	       attrs
	       (merge-adjacent-strings
		(mapcar (papply (lhtml-filter ? :name name :id id :class class))
			(remove-if #'(lambda (element)
				       (and (not (stringp element))
					    (lhtml-matches? element :name name :id id :class class)))
				   contents)))))))

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

(defun-web fetch-svd.se-article (url)
  (let ((html (lhtml-filter (parse-html (drakma-request url nil))
			    :name :script)))
    (list (cons :source "Svenska Dagbladet")
	  (cons :url url)
	  (cons :language :swedish)
	  (cons :retrieved (get-universal-time))
	  (cons :text (cleanup-whitespace
		       (lhtml->text (lhtml-select html
						  :name :p
						  :class "preamble"))
		       (lhtml->text (lhtml-select html
						  :name :div
						  :class "articletext"))))
	  (cons :author (string-join " "
				     (cleanup-whitespace (lhtml->text (lhtml-select html
										    :name :p
										    :class "author")))))
	  (cons :title (string-join " "
				    (cleanup-whitespace (lhtml->text (lhtml-select (lhtml-select html
												 :name :div
												 :id "article")
										   :name :h1))))))))

(defmacro def-media-fetcher ((front-page-function article-function base-url &key url-regex source-name language (url-prefix ""))
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
			  (when (cl-ppcre:scan url-type href)
			    (collect ,(if url-prefix
					  `(concatenate 'string
							,url-prefix
							href)
					  'href)))))))
		  :test #'equal)
		 #',article-function
		 ,language)))
     (push #',front-page-function *front-page-fetchers*)))
				 

(defun-web fetch-svd.se-front-page ()
  (let ((url-type (cl-ppcre:create-scanner "^http://www.svd.se/(?:[a-z]+/)+[\\w-]+_[0-9]+\\.svd$")))
    (values (remove-duplicates 
	     (collecting
	       (dolist (entry (lhtml-select (parse-html (drakma-request "http://www.svd.se" nil))
					    :list t
					    :name :a))
		 (destructuring-bind (name attrs . contents)
		     entry
		   (declare (ignore name contents))
		   (let ((href (second (assoc :href attrs))))
		     (when (cl-ppcre:scan url-type href)
		       (collect href))))))
	     :test #'equal)
	    #'fetch-svd.se-article
	    :swedish)))
  
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

(defun make-fetch-from-front-pages-continuation (fetchers &key languages)
  #'(lambda (reschedule-continuation done-continuation)
      (macrolet ((future (&body body)
		   `#'(lambda (reschedule-continuation done-continuation)
			,@body))
		 (go-on (future)
		   `(funcall reschedule-continuation
			     5.0
			     ,future)))
	(log-info 'fetch-from-front-pages
		  "fetching from front pages (languages ~a): ~a" languages fetchers)
	(labels ((process-fetchers (fetchers)
		   (destructuring-bind (fetcher . rest-of-fetchers)
		       fetchers
		     (multiple-value-bind (urls article-fetcher language)
			 (funcall fetcher)
		       (let ((will-continue (or (null languages)
						(find language languages))))
		       (log-info 'fetch-from-front-pages-continuation
				 "checked front page ~a, ~a results in ~a (continuing? ~a)"
				 fetcher
				 (length urls)
				 language
				 will-continue)
		       (labels ((out-of-urls ()
				  (if rest-of-fetchers
				      (go-on (future (process-fetchers rest-of-fetchers)))
				      (funcall done-continuation))))
			 (if (and urls will-continue)
			     (go-on (future (process-urls article-fetcher
							  urls
							  #'out-of-urls)))
			     (funcall #'out-of-urls)))))))
	       (process-urls (article-fetcher urls done-continuation)
		 (if (null urls)
		     (funcall done-continuation)
		     (destructuring-bind (url . rest-of-urls)
			 urls
		       (multiple-value-bind (resource accessed-network)
			   (fetch-web-resource article-fetcher url)
			 (declare (ignore resource))
			 (if rest-of-urls
			     (funcall reschedule-continuation
				      (if accessed-network
					  5.0
					  0)
				      (future (process-urls article-fetcher rest-of-urls done-continuation)))
			     (funcall done-continuation)))))))
	(process-fetchers fetchers)))))

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

(defun-web article-sentences (text)
  (when (consp text)
    (return-from article-sentences
      (apply #'append
	     (mapcar #'article-sentences
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
    (dolist (sentence (article-sentences text))
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
	      