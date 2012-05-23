(in-package :wiktionary-bot)

(defparameter *user-agent* "Metallmanul Wiktionary bot (by svk)")
(defparameter *wiktionary-language* "sv")

(defvar *cookies* (make-instance 'drakma:cookie-jar))

(defun read-login-info (&optional (filename #p"./data/bot-login.info"))
  (let ((*read-eval* nil))
    (with-open-file (f filename :direction :input)
      (let ((alist (read f)))
	(values (cdr (assoc :username alist))
		(cdr (assoc :password alist)))))))

(defun drakma-request (url parameters &rest args)
  (apply #'drakma:http-request
	 url
	 :parameters parameters
	 :cookie-jar *cookies*
	 args))

(defun fetch-json (url parameters &rest http-request-args)
  (json:decode-json
   (apply #'drakma-request
	  url
	  parameters
	  :want-stream t
	  http-request-args)))
		    

(defun fetch-lhtml (url parameters &rest http-request-args)
  (chtml:parse (apply #'drakma-request
		      parameters
		      url
		      http-request-args)
	       (chtml:make-lhtml-builder)))

(defun lhtml-matches? (tag &key name id class)
  (and (or (null name)
	   (eq (car tag) name))
       (or (null id)
	   (equalp (cadr (assoc :id (cadr tag)))
		   id))
       (or (null class)
	   (find class
		 (split-sequence #\space (cadr (assoc :class (cadr tag))))
		 :test #'equalp))))

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

(defun build-api-url (&key use-https)
  (concatenate 'string
	       (if use-https
		   "https"
		   "http")
	       "://"
	       *wiktionary-language*
	       ".wiktionary.org/w/api.php?"))

(defun build-parameters (&rest parameters)
  (collecting
    (do* ((rest parameters)
	  (keyword (pop rest) (pop rest))
	  (value (pop rest) (pop rest)))
	 ((or (null keyword)
	      (null value))
	  (when keyword
	    (error "odd number of keyword parameters")))
      (collect (cons (to-url-part keyword)
		     (to-url-part value))))))

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
	     (equal "Success"
		    (cdr (assoc :result
				(cdr (assoc :login
					    (api-post :action :login
						      :lgname username
						      :use-https t
						      :lgpassword password
						      :lgtoken (cdr (assoc :token response)))))))))
	    (t nil)))))

(defun api (&rest args &key use-https use-post &allow-other-keys)
  (setf args (remove-keyword-parameter :use-https args))
  (setf args (remove-keyword-parameter :use-post args))
  (fetch-json (build-api-url :use-https use-https)
	      (apply #'build-parameters
		     :format :json
		     args)
	      :method (if use-post :post :get)))

(defun api-post (&rest args)
  (apply #'api
	 :use-post t
	 args))

(defun api-get (&rest args)
  (apply #'api args))

(defun api-watchlist-raw ()
  (api-get :action :query
	   :list :watchlistraw))

(defun api-logout () (api-get :action :logout))

(defun api-query (&rest args)
  (apply #'api-get
	 :action :query
	 args))

(defun api-revisions (titles &key (prop '("content")))
  (api-query :prop :revisions
	     :titles titles
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

(defun page-source (title)
  (extract (list :query :pages #'cdar :revisions 0 :*)
	   (api-revisions title
			  :prop "content")))