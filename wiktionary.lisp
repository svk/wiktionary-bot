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

(defun drakma-request (url &rest args)
  (apply #'drakma:http-request
	 url
	 :cookie-jar *cookies*
	 args))

(defun fetch-json (url &rest http-request-args)
  (json:decode-json
   (apply #'drakma-request
	  url
	  :want-stream t
	  http-request-args)))
		    

(defun fetch-lhtml (url &rest http-request-args)
  (chtml:parse (apply #'drakma-request
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
  (fetch-lhtml url))

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

(defun build-api-url (&rest parameters)
  (concatenate 'string
	       "http://"
	       *wiktionary-language*
	       ".wiktionary.org/w/api.php?"
	       (string-join "&"
			    (collecting
			      (do* ((rest parameters)
				    (keyword (pop rest) (pop rest))
				    (value (pop rest) (pop rest)))
				   ((or (null keyword)
					(null value))
				    (when keyword
					(error "odd number of keyword parameters")))
				(collect (concatenate 'string
						      (to-url-part keyword)
						      "="
						      (to-url-part value))))))))

  
(defun fetch-wiki-page (word)
  (fetch-lhtml (build-wiki-url word)))

(defun api-login ()
  (multiple-value-bind (username password)
      (read-login-info)
    (let* ((response (cdr (assoc :login (api-post :action :login
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
						      :lgpassword password
						      :lgtoken (cdr (assoc :token response)))))))))
	    (t nil)))))

(defun api-get (&rest args)
  (fetch-json (apply #'build-api-url
		     :format :json
		     args)))

(defun api-post (&rest args)
  (fetch-json (apply #'build-api-url
		     :format :json
		     args)
	      :method :post))

(defun api-watchlist-raw ()
  (api-get :action :query
	   :list :watchlistraw))

(defun api-logout () (api-get :action :logout))

(defun api-query (&rest args)
  (apply #'api-get
	 :action :query
	 args))

(defun api-revisions (page)
  (api-query :prop :revisions
	     :titles page
	     :rvprop '("content")))
  