(in-package :wiktionary-bot)

(defparameter *user-agent* "MetallmanulBot for Wiktionary (run by Metallmanul)")
(defparameter *wiktionary-language* "sv")

(defvar *cookies* (make-instance 'drakma:cookie-jar))

(defparameter *delay-read* 0.5)
(defparameter *delay-write* 5.0)

(defparameter *edit-count* 0)

(defparameter *edit-log-filename* #p"./data/edited-pages-log.generated.lisp")

(defparameter *edited-pages* (let ((ht (make-hash-table :test #'equal)))
			       (with-open-file (f *edit-log-filename* :direction :input :if-does-not-exist nil)
				 (when f
				   (loop
				      :for x = (let ((*read-eval* nil)) (read f nil nil))
				      :until (null x)
				      :do (setf (gethash (first x) ht) (second x)))))
			       ht))

(defvar *post-edit-hooks* nil)

(defun register-in-edit-log (title timestamp summary)
  (setf (gethash title *edited-pages*) timestamp)
  (with-open-file (*standard-output* *edit-log-filename* :direction :output :if-exists :append)
    (write (list title timestamp summary))))

(defun in-edit-log? (title)
  (multiple-value-bind (timestamp present-p)
      (gethash title *edited-pages*)
    (declare (ignore timestamp))
    present-p))

(define-condition edit-failed (error)
  ((text :initarg :text :reader text)
   (result :initarg :result :reader wiki-failure-result)))

(define-condition login-failed (error)
  ((text :initarg :text :reader text)
   (result :initarg :result :reader wiki-failure-result)))

(define-condition api-error (error)
  ((text :initarg :text :reader text)
   (code :initarg :code :reader code :initform nil)
   (result :initarg :result :reader wiki-failure-result)))

(define-condition article-already-exists (api-error) ())

(define-condition operation-failed (error)
  ((text :initarg :text :reader text)))


(defun wait-read ()
  (sleep *delay-read*))

(defun wait-write ()
  (sleep *delay-write*))

(defun read-login-info (&optional (filename #p"./data/bot-login.info"))
  (let ((*read-eval* nil))
    (with-open-file (f filename :direction :input)
      (let ((alist (read f)))
	(values (cdr (assoc :username alist))
		(cdr (assoc :password alist)))))))

(defun drakma-request (url parameters &rest args)
  (let ((flex:*substitution-char* (code-char 65533)))
    (let ((rv (apply #'drakma:http-request
		     url
		     :external-format-in :utf-8
		     :external-format-out :utf-8
		     :user-agent *user-agent*
		     :parameters parameters
		     :cookie-jar *cookies*
		     args)))
      (wait-read)
      rv)))

(defun fetch-json (url parameters &rest http-request-args)
  (let ((stream (apply #'drakma-request
			url
			parameters
			:want-stream t
			http-request-args)))
    (if (null stream)
	(error 'operation-failed
	       :text "null stream from request")
	(unwind-protect
	     (json:decode-json stream)
	  (close stream)))))
		  
(defun parse-html (html)
  (chtml:parse html (chtml:make-lhtml-builder)))

(defun fetch-lhtml (url parameters &rest http-request-args)
  (parse-html (apply #'drakma-request
		     parameters
		     url
		     http-request-args)))

(defun lhtml-matches? (tag &key name id class)
  (labels ((ok (x) (find x
			 (split-sequence #\space (cadr (assoc :class (cadr tag))))
			 :test #'equalp)))
    (and (or (null name)
	     (eq (car tag) name))
	 (or (null id)
	     (equalp (cadr (assoc :id (cadr tag)))
		     id))
	 (or (null class)
	     (if (listp class)
		 (every #'ok class)
		 (ok class))))))

(defun lhtml-select (root &key name id class list)
  (if (or (null root)
	  (stringp root))
      nil
      (do* ((stack (list root))
	    (current (pop stack) (pop stack))
	    (matching))
	   ((null current) (reverse matching))
	(when (consp current)
	  (when (lhtml-matches? current :name name :id id :class class)
	    (when (not list)
	      (return-from lhtml-select current))
	    (push current matching))
	  (dolist (tag (cddr current))
	    (push tag stack))))))
  
	
(defun fetch-example-site (&optional (url "http://sv.wiktionary.org/wiki/katt"))
  (fetch-lhtml url nil))

(defun build-wiki-url (page)
  (concatenate 'string
	       "http://"
	       *wiktionary-language*
	       ".wiktionary.org/wiki/"
	       page))

(defun to-url-part (x)
  (cond ((stringp x) x)
	((symbolp x) (string-downcase (symbol-name x)))
	((null x) "")
	((listp x) (string-join "|" (mapcar #'to-url-part x)))
	(t (error (format nil "not sure how to encode ~a" x)))))

(defun temporary-error (type &optional atom)
  (error (format nil "temporary errors not yet implemented: ~a, ~a" type atom)))

(defun remove-keyword-parameter (parameter parameter-list)
  (collecting
    (do* ((rest parameter-list)
	  (keyword (pop rest) (pop rest))
	  (value (pop rest) (pop rest)))
	 ((or (null keyword)))
      (unless (eq parameter keyword)
	(collect keyword)
	(collect value)))))

(defun ->keyword (x)
  (intern (cond ((stringp x) (string-upcase x))
		(t x)) :keyword))

(defun build-api-url (&key use-https (site "wiktionary.org"))
  (concatenate 'string
	       (if use-https
		   "https"
		   "http")
	       "://"
	       *wiktionary-language*
	       "."
	       site
	       "/w/api.php?"))

(defun build-parameters (&rest parameters)
  (collecting
    (do* ((rest parameters)
	  (keyword (pop rest) (pop rest))
	  (value (pop rest) (pop rest)))
	 ((and (or (null keyword)
		   (null value))
	       (null rest))
	  (when keyword
	    (error "odd number of keyword parameters")))
      (cond ((eq value nil))
	    ((eq value t)
	     (collect (cons (to-url-part keyword) "")))
	    (t
	     (collect (cons (to-url-part keyword)
			    (to-url-part value))))))))

(defun fetch-wiki-page (word)
  (fetch-lhtml (build-wiki-url word) nil))

(defun api-login ()
  (multiple-value-bind (username password)
      (read-login-info)
    (let* ((response (cdr (assoc :login (api-post :action :login
						  :use-https t
						  :lgname username
						  :lgpassword password))))
	   (result (cdr (assoc :result response))))
      (cond ((equal result "Success") t)
	    ((equal result "NeedToken")
	     (if (equal "Success"
			(cdr (assoc :result
				    (cdr (assoc :login
						(api-post :action :login
							  :lgname username
							  :use-https t
							  :lgpassword password
							  :lgtoken (cdr (assoc :token response))))))))
		 t
		 (error 'login-failed
			:text "login failed (after token)"
			:result result)))
	    (t (error 'login-failed
		      :text "login failed (before token)"
		      :result result))))))

(defun signal-if-api-error (result)
  (let ((code (extract '(:error :code) result)))
    (if (null code)
	result
	(error (cond ((equal code "articleexists") 'article-already-exists)
		     (t 'api-error))
	       :text (format nil "API error (~a)" (extract '(:error :info) result))
	       :code code
	       :result result))))
	     

(defun api (&rest args &key use-https use-post (retries 5) (retry-delay 60) &allow-other-keys)
  (setf args (remove-keyword-parameter :use-https args))
  (setf args (remove-keyword-parameter :retries args))
  (setf args (remove-keyword-parameter :retry-delay args))
  (setf args (remove-keyword-parameter :use-post args))
  (handler-case
      (signal-if-api-error (fetch-json (build-api-url :use-https use-https)
				       (apply #'build-parameters
					      :format :json
					      :maxlag "1"
					      args)
				       :method (if use-post :post :get)))
    ((and api-error (not (or article-already-exists)))
	(condition)
      (sleep retry-delay)
      (when (zerop retries)
	(signal condition))
      (return-from api (apply #'api
				:use-https use-https
				:use-post use-post
				:retries (1- retries)
				:retry-delay (* 2 retry-delay)
				args)))))
	       
(defun wikipedia-api (&rest args &key use-https use-post &allow-other-keys)
  (setf args (remove-keyword-parameter :use-https args))
  (setf args (remove-keyword-parameter :use-post args))
  (fetch-json (build-api-url :use-https use-https :site "wikipedia.org")
	      (apply #'build-parameters
		     :format :json
		     :maxlag "1"
		     args)
	      :method (if use-post :post :get)))

(defun api-post (&rest args)
  (apply #'api
	 :use-post t
	 args))

(defun wikipedia-api-post (&rest args)
  (apply #'wikipedia-api
	 :use-post t
	 args))

(defun api-get (&rest args)
  (apply #'api args))

(defun wikipedia-api-get (&rest args)
  (apply #'wikipedia-api args))

(defun api-watchlist-raw ()
  (api-get :action :query
	   :list :watchlistraw))

(defun api-logout () (api-get :action :logout))

(defun api-query (&rest args)
  (apply #'api-get
	 :action :query
	 args))

(defun wikipedia-api-query (&rest args)
  (apply #'wikipedia-api-get
	 :action :query
	 args))

(defun api-revisions (titles &key (prop '("content")) parse)
  (api-query :prop :revisions
	     :titles titles
	     :rvparse parse
	     :rvprop prop))

(defun wikipedia-api-revisions (titles &key (prop '("content")) parse)
  (wikipedia-api-query :prop :revisions
		       :titles titles
		       :rvparse parse
		       :rvprop prop))

(defun api-expandtemplates (text)
  (api-get :action :expandtemplates
	   :text text))

(defun extract (path structure)
  (dolist (element path)
    (setf structure
	  (cond ((integerp element)
		 (nth element structure))
		((symbolp element)
		 (cdr (assoc element structure)))
		((functionp element)
		 (funcall element structure))
		(t (error (format nil "unable to extract with element ~a" element))))))
  structure)

(defun expand-templates (text)
  (extract '(:expandtemplates :*)
	   (api-expandtemplates text)))

(defun expand-templates-on-page (title text)
  (extract '(:expandtemplates :*)
	   (api-get :action :expandtemplates
		    :title title
		    :text text)))

(defun page-source (title)
  (extract (list :query :pages #'cdar :revisions 0 :*)
	   (api-revisions title
			  :prop "content")))

(defun wikipedia-page-source (title)
  (extract (list :query :pages #'cdar :revisions 0 :*)
	   (wikipedia-api-revisions title
				    :prop "content")))

(defun page-rendered (title)
  (let ((page-parsed (extract (list :query :pages #'cdar :revisions 0 :*)
			      (api-revisions title
					     :parse t
					     :prop "content"))))
    (when page-parsed
      (parse-html page-parsed))))

(defun parse-simple-bullet-list (markup)
  (remove-if #'(lambda (x) (equal x ""))
	     (mapcar #'trim
		     (split-sequence #\* markup))))

(defparameter *disable-all-caching* nil) ;;careful!

(defmacro def-simple-cached (name &body body)
  (let ((cache-sym (gensym "CACHE")))
    `(let ((,cache-sym))
       (defun ,name (&key (force *disable-all-caching*))
	 (or (and (not force) ,cache-sym)
	     (setf ,cache-sym
		   (progn ,@body)))))))

(def-simple-cached swedish-blessed-grammar-templates
  (let ((*wiktionary-language* "sv"))
    (parse-simple-bullet-list
     (expand-templates "{{Wiktionary:Användare/Robotar/Godkända_grammatikmallar}}"))))       

(defun create-regexes-from-template-names (template-names)
  (loop
     :for template-name :in template-names
     :collect
     (list 
      (let ((regex (concatenate 'string
				"{{"
				(cl-ppcre:regex-replace-all
				 "([()])"
				 template-name
				 "\\\\\\1")
				"(|.*?)*?}}")))
	(cl-ppcre:create-scanner regex))
      (concatenate 'string
		   "template-"
		   template-name))))

(def-simple-cached swedish-grammar-table-regexes
  (create-regexes-from-template-names (swedish-blessed-grammar-templates)))

(defun scan-for-grammar-tables (regex-list page &key double-check fully-live early-warning)
  (when (and early-warning
	     (swedish-dump-text page)
	     (or (swedish-dump-text (concatenate 'string page "en"))
		 (swedish-dump-text (concatenate 'string page "t"))
		 (swedish-dump-text (concatenate 'string page "r"))))
    (return-from scan-for-grammar-tables nil))
  (let ((before (if (or fully-live
			(not (swedish-dump-text page)))
		    (page-source page)
		    (swedish-dump-text page)))
	(rendered (page-rendered page)))
    (when double-check
      (let ((after (page-source page)))
	(when (not (equal before after))
	  (temporary-error :edited-during-scan page))))
    (when (and before rendered)
      (collecting 
	(dolist (element regex-list)
	  (destructuring-bind (scanner class)
	      element
	    (when (cl-ppcre:scan scanner before)
	      (let ((tables (lhtml-select rendered :name :table :class (list class "grammar") :list t)))
		(when (eql (length tables) 1)
		  (collect (car tables)))))))))))

(defun layout-table (table)
  (let ((no-cols)
	(no-rows))
    (values
     (collecting
       (let ((ht (make-hash-table :test #'equal)))
	 (labels ((cell? (x) (or (eq (car x) :th)
				 (eq (car x) :td)))
		  (row? (x) (eq (car x) :tr))
		  (tag-colspan (tag)
		    (let ((x (cadr (assoc :colspan (cadr tag)))))
		      (if (null x)
			  1
			  (parse-integer x))))
		  (tag-rowspan (tag)
		    (let ((x (cadr (assoc :rowspan (cadr tag)))))
		      (if (null x)
			  1
			  (parse-integer x))))
		  (parse-cell (col col-span row row-span element)
		    (loop
		       :until (not (gethash (cons col row) ht))
		       :do (incf col))
		    (loop
		       :for x :from col :to (1- (+ col col-span))
		       :do
		       (loop
			  :for y :from row :to (1- (+ row row-span))
			  :do (setf (gethash (cons x y) ht) t)
			  :do (collect (list x y element))))
		    (+ col col-span))
		  (parse-row (row elements)
		    (let ((col 0))
		      (dolist (cell (remove-if-not #'cell? elements))
			(setf col (parse-cell col
					      (tag-colspan cell)
					      row
					      (tag-rowspan cell)
					      (cddr cell))))))
		  (parse-rows (elements)
		    (let ((row 0))
		      (dolist (cell (remove-if-not #'row? elements))
			(parse-row row (cddr cell))
			(incf row (tag-rowspan cell))))))
	   (parse-rows (cddr (lhtml-select table :name :tbody)))
	   (let* ((keys (hash-table-keys ht))
		  (cols (reduce #'max (mapcar #'car keys)))
		  (rows (reduce #'max (mapcar #'cdr keys))))
	     (setf no-cols (1+ cols)
		   no-rows (1+ rows))))))
     no-cols
     no-rows)))

(defun visualize-table (values cols rows)
  (let ((xs)
	(arr (make-array (list rows cols))))
    (labels ((visual-value-of (x)
	       (1+ (or (position x xs)
		       (1- (length (setf xs (append xs (list x)))))))))
      (dotimes (row rows)
	(dotimes (col cols)
	  (setf (aref arr row col)
		(visual-value-of (caddr (find (cons col row)
					      values
					      :key #'(lambda (z) (cons (car z) (cadr z)))
					      :test #'equal))))))
      arr)))

(defun merge-adjacent-strings (things)
  (reduce #'(lambda (a b)
	      (if (and (stringp a)
		       (stringp (first b)))
		  (cons (concatenate 'string a (first b)) (rest b))
		  (cons a b)))
	  (append things (list nil))
	  :from-end t))

(defun simplify-html (cell)
  (cond ((and (consp cell)
	      (keywordp (car cell)))
	 (collecting 
	   (loop
	      :for element :in (mapcar #'simplify-html-2 (cddr cell))
	      :do
	      (loop :for x :in element :do (collect x)))))
	((stringp cell)
	 (list cell))
	(t nil)))

(defun simplify-cells (cells)
  (mapcar #'trim-if-string
	  (merge-adjacent-strings
	   (apply #'nconc
		  (remove-if-not #'identity (mapcar #'simplify-html cells))))))

(defun trim-if-string (cell)
  (if (stringp cell)
      (trim cell)
      cell))

(defun find-textual-cell-contents (cells)
  (and (not (lhtml-select (list* :virtual nil cells) :name :a))
       (simplify-cells cells)))

(defun find-link-cell-contents (cells)
  (lhtml-select (list* :virtual nil cells) :name :a))

(defun select-layouted-cell (cells x y)
  (caddr (find (list x y) cells :test #'equal :key (papply (first-n ? 2)))))

(defun lhtml->text-list (elements)
  (let ((newline "
"))
    (cond ((listp elements)
	 (if (keywordp (car elements))
	     (destructuring-bind (tag attrs . rest)
		 elements
	       (declare (ignore attrs))
	       (cond ((eq tag :br)
		      (list newline))
		     ((eq tag :p)
		      (append (list newline) (lhtml->text-list rest) (list newline)))
		     (t (lhtml->text-list rest))))
	     (apply #'nconc
		    (mapcar #'lhtml->text-list elements))))
	((stringp elements) (list elements))
	(t))))

(defun lhtml->text (elements)
  (apply #'concatenate
	 'string
	 (lhtml->text-list elements)))

(defmacro def-table-recognizer (name value-map-f table-rows)
  (let* ((table-height (length table-rows))
	 (table-width (length (car table-rows)))
	 (checks)
	 (targets))
    (loop
       :for row :in table-rows
       :for y :from 0
       :do
       (loop
	  :for col :in row
	  :for x :from 0
	  :do
	  (cond ((null col))
		((stringp col)
		 (push (list x y col) checks))
		(t
		 (push (list x y col) targets)))))
    (assert (every (compose (papply (eql ? table-width)) #'length) table-rows))
    `(defun ,name (rows)
       (multiple-value-bind (rows width height)
	   (layout-table rows)
	 (cond ((not (eql height ,table-height))
		nil)
	       ((not (eql width ,table-width))
		nil)
	       ,@(loop
		    :for (x y string) :in checks
		    :collect `((not (search ,string
					    (lhtml->text (select-layouted-cell rows ,x ,y))))
			       nil))
	       (t (collecting
		    ,@(loop
			 :for (x y target) :in targets
			 :collect `(let ((value (funcall ,value-map-f
							 (select-layouted-cell rows ,x ,y))))
				     (when value (collect (list (list ,@target) value))))))))))))

(defun format-timestamp (&optional (timestamp (get-universal-time)))
  (format nil "~a" (rfc3339:make-timestamp :utc-time timestamp)))

(defun api-edit (title summary &key mutator prepend append minor (bot t) section createonly (retries 5) (retry-delay 5))
  (assert (or mutator prepend append))
  (assert (not (and mutator
		    (or prepend append))))
  (labels ((retry (result)
	     (when (not (plusp retries))
	       (error 'edit-failed
		      :text (format nil "edit failed, retries exhausted (~a)" result)
		      :result result))
	     (api-login)
	     (api-edit title
		       summary
		       :mutator mutator
		       :prepend prepend
		       :append append
		       :minor minor
		       :bot bot
		       :section section
		       :createonly createonly
		       :retries (1- retries)
		       :retry-delay (* 2 retry-delay))))
    (let* ((result (api :action :query
			:prop '(:info :revisions)
			:intoken :edit
			:titles title
			:rvprop (append (if mutator
					    '(:content)
					    nil)
					'(:timestamp)))))
      (let ((source (extract (list :query :pages #'cdar :revisions 0 :*)
			     result))
	    (start-timestamp (extract (list :query :pages #'cdar :starttimestamp)
				      result))
	    (base-timestamp (extract (list :query :pages #'cdar :revisions 0 :timestamp)
				     result))
	    (edit-token (extract (list :query :pages #'cdar :edittoken)
				 result)))
	(when edit-token
	  (let ((result (apply #'api
			       :use-post t
			       :action :edit
			       :title title
			       :summary summary
			       :section section
			       :starttimestamp start-timestamp
			       :basetimestamp base-timestamp
			       :createonly createonly
			       :minor minor
			       :bot bot
			       :assert "bot"
			       (append (if mutator
					   (list :text (or (funcall mutator source)
							   (error "mutator returned NIL")))
					   (append (when append (list :appendtext append))
						   (when prepend (list :prependtext prepend))))
				       (list :token edit-token)))))
	    (wait-write)
	    (let ((result-status (extract '(:edit :result) result)))
	      (if (equal result-status "Success")
		  (progn (incf *edit-count*)
			 (register-in-edit-log title (get-universal-time) summary)
			 (dolist (hook *post-edit-hooks*)
			   (funcall hook))
			 result)
		  (if (equal result-status "Failure")
		      (retry result)
		      (error 'edit-failed
			     :text "result of edit unclear"
			     :result result))))))))))
			     
