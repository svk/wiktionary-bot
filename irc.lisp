(in-package :wiktionary-bot)

(define-module-lock *irc-lock*
    with-irc
  defun-irc)

(defparameter *default-nickname* "MetallManul")
(defparameter *default-realname* "MetallManul")
(defparameter *default-username* "MetallManul")
(defparameter *default-channels* '("#MetallManulBot"))

(defparameter *irc-report-channel* "#MetallManulBot")

(defparameter *command-token* "(")
(defparameter *skip-command-token* nil)

(defparameter *irc-values* (make-hash-table :test #'equal))

(defmacro defirc (name args &body body)
  `(defun-irc ,name ,args
     (when *irc-process*
       (if *in-irc-thread?*
	   (progn ,@body nil)
	   (mp:process-interrupt *irc-process*
				 #'(lambda ()
				     ,@body))))))

(defirc irc-join (channel)
  (cl-irc:join *irc-connection* channel))

(defirc irc-op (channel target)
  (cl-irc:op *irc-connection* channel target))

(defirc irc-nick (channel)
  (cl-irc:nick *irc-connection* channel))

(defirc irc-part (channel &optional reason)
  (cl-irc:part *irc-connection* channel reason))

(defirc irc-quit (message)
  (cl-irc:quit *irc-connection* message))

(defirc irc-privmsg (channel message)
  (cl-irc:privmsg *irc-connection* channel message))

(defparameter *irc-persistent-storage* #p"./data/irc-storage.lisp")
			    
(defun-irc irc-get-value (name)
  (gethash name *irc-values*))

(defun-irc irc-set-value (name value)
  (setf (gethash name *irc-values*) value))

(defvar *irc-connection* nil)
(defvar *irc-process* nil)

(defun-irc parse-irc-input (text)
  (when (begins-with *command-token* text)
    (let ((*read-eval* nil)
	  (*readtable* *irc-readtable*))
      (collecting
	(with-input-from-string (f (subseq text (if *skip-command-token*
						    (length *command-token*)
						    0)))
	  (handler-case (loop :do (collect (read f)))
	    (end-of-file ())))))))

(defun-irc begin-irc (&optional (filename #p"./data/bot-irc.info"))
  (let ((*read-eval* nil))
    (with-open-file (f filename :direction :input)
      (let ((alist (read f)))
	(begin-irc-as :server (cdr (assoc :server alist))
		      :password (cdr (assoc :password alist))
		      :port (cdr (assoc :port alist))
		      :channels (or (cdr (assoc :channels alist)) *default-channels*)
		      :nickname (or (cdr (assoc :nickname alist)) *default-nickname*)
		      :realname (or (cdr (assoc :realname alist)) *default-realname*)
		      :username (or (cdr (assoc :username alist)) *default-username*))))))



(defparameter *in-irc-thread?* nil)
(defparameter *irc-readtable* (let ((table (copy-readtable)))
				(setf (readtable-case table) :preserve)
				table))

(defparameter *source-nick* nil)
(defparameter *source-host* nil)
(defparameter *source-channel* nil)
(defparameter *reply-target* nil)

(defun-irc report-editcounts (&rest users)
  (let ((users (or users (list "MetallmanulBot"))))
    (irc-reply-format "Requesting edit counts for: ~a" (string-join " " users))
    (dolist (entry (extract '(:query :users) (api-query :list :users :ususers users :usprop :editcount)))
      (irc-reply-format "[~a/~a]: ~a"
			 (extract '(:name) entry)
			 (extract '(:userid) entry)
			 (extract '(:editcount) entry)))))

(define-condition irc-command-error (error)
  ((text :reader text :initarg :text)))

(defun-irc bot-op (&rest targets)
  (if (null targets)
      (setf targets (list *source-nick*)))
  (when *source-channel*
    (dolist (target targets)
      (irc-op *source-channel* target))))  

(defparameter *blessed-functions* (list-in-file #p"./data/irc-blessed-functions.lisp"))

(defparameter *irc-function-table* (let ((ht (make-hash-table :test #'equal)))
				     (dolist (fs *blessed-functions*)
				       (if (consp fs)
					   (setf (gethash (string-upcase (symbol-name (first fs))) ht)
						 (symbol-function (second fs)))
					   (setf (gethash (string-upcase (symbol-name fs)) ht)
						 (symbol-function fs))))
				     ht))


(defmacro spawn-with-preserved-context (name preserved &body body)
  (let ((equivalents (mapcar #'(lambda (x) (cons (gensym) x)) preserved)))
    `(let (,@(loop :for (a . b) :in equivalents :collect (list a b)))
       (mp:process-run-function ,name
				#'(lambda ()
				    (let (,@(loop :for (a . b) :in equivalents :collect (list b a)))
				      ,@body))))))

(defmacro spawn-with-irc-context (name &body body)
  `(spawn-with-preserved-context ,name
       (*irc-readtable* *source-nick* *source-host* *source-channel* *reply-target* *blessed-functions* *irc-function-table*)
     ,@body))

(defun-irc irc-function (name)
  (let ((name (string-upcase (symbol-name name))))
    (or (gethash name *irc-function-table*)
	(signal 'irc-command-error :text (format nil "function not found: ~a" name)))))


(defun-irc irc-eval (sexp)
  (cond ((or (stringp sexp)
	     (numberp sexp)) sexp)
	((symbolp sexp) (symbol-name sexp))
	((consp sexp)
	 (destructuring-bind (head . args)
	     sexp
	   (case (intern (string-upcase (symbol-name head)))
	     (delay (destructuring-bind (seconds* . rest)
			args
		      (spawn-with-irc-context "Delayed execution thread"
			(sleep (irc-eval seconds*))
			(dolist (sexp rest)
			  (irc-eval sexp)))))
	     (otherwise (apply (irc-function head)
			       (mapcar #'irc-eval args))))))
	(t (signal 'irc-command-error "unexpected sexp ~a" sexp))))
		       
(defun-irc irc-do (sexp)
  (handler-case
      (let ((result (irc-eval sexp)))
	(when result
	  (irc-reply-format "~a: ==> ~a" *source-nick* result)))
    (irc-command-error (condition)
      (irc-reply-format "~a: error -- ~a" *source-nick* (with-slots (text) condition text)))))

(defun-irc irc-input (message)
  (in-package :wiktionary-bot)
  (let* ((source-host (cl-irc:host message))
	 (source-nick (cl-irc:source message))
	 (target (first (cl-irc:arguments message)))
	 (text (second (cl-irc:arguments message)))
	 (connection  (cl-irc:connection message))
	 (target-user (cl-irc:find-user connection target))
	 (private? (equal target (cl-irc:nickname (cl-irc:user connection))))
	 (*reply-target* (if private?
			     source-nick
			     target)))
    (ignore-errors (let ((sexps (parse-irc-input text)))
		     (dolist (sexp sexps)
		       (when sexp
			 (let ((*source-nick* source-nick)
			       (*source-host* source-host)
			       (*source-channel* (if private? nil target)))
			   (irc-do sexp))))
		     sexps))))

(defun-irc irc-input-base (message) (irc-input message))

(defun-irc begin-irc-as (&key server port password (nickname "MetallManulBot") (channels '("#MetallManulBot")) (realname "MetallmanulBot") (username "metallmanulbot") foreground)
  (let ((connection (cl-irc:connect :username username
				    :realname realname
				    :nickname nickname
				    :port port
				    :server server
				    :password password)))
    (dolist (channel channels)
      (if (consp channel)
	  (cl-irc:join connection (car channel) :password (cdr channel))
	  (cl-irc:join connection channel)))
    (cl-irc:add-hook connection 'cl-irc:irc-privmsg-message #'irc-input-base)
    (setf *irc-connection* connection)
    (if foreground
	(unwind-protect (cl-irc:read-message-loop connection)
	  (ignore-errors (irc-quit "Foreground quit")))
	(setf *irc-process* (mp:process-run-function "IRC thread"
						     #'(lambda ()
							 (let ((*in-irc-thread?* t))
							   (cl-irc:read-message-loop connection))))))))

(defun-irc stop-irc ()
  (ignore-errors (irc-quit "Stopping"))
  (setf *irc-connection* nil)
  (when *irc-process*
    (mp:process-kill *irc-process*))
  (setf *irc-process* nil))

(defun-irc restart-irc ()
  (stop-irc)
  (begin-irc))

(defun-irc number-of-articles ()
  (extract '(:query :statistics :articles) (api-query :meta :siteinfo :siprop :statistics)))

(defun-irc message-channel-articles (channel)
  (irc-privmsg channel
	       (format nil
		       "~a: ~a articles" 
		       (rfc3339:make-timestamp)
		       (number-of-articles))))


(defparameter *irc-line-length* 160)

(defparameter *irc-split-suffix* "…")
(defparameter *irc-split-prefix* "    ")

(defun-irc %irc-split-line (text &optional prefix)
  (if (<= (length text) *irc-line-length*)
      (list (concatenate 'string
			 (if prefix *irc-split-prefix* "")
			 text))
      (let ((content-len (- *irc-line-length* (length *irc-split-suffix*) (if prefix (length *irc-split-prefix*) 0))))
	(cons (concatenate 'string
			   (if prefix *irc-split-prefix* "")
			   (subseq text 0 content-len)
			   *irc-split-suffix*)
	      (%irc-split-line (subseq text content-len) t)))))

(defun-irc irc-split-line (text) (%irc-split-line text))
(defun-irc irc-report-format (message &rest args)
  (irc-report (let ((*print-pretty* nil)) (apply #'format nil message args))))

(defun-irc irc-report (message)
  (dolist (message (irc-split-line message))
    (irc-privmsg *irc-report-channel*
		 message)))

(defun-irc irc-reply-format (message &rest args)
  (irc-reply (let ((*print-pretty* nil)) (apply #'format nil message args))))

(defun-irc irc-reply (message)
  (dolist (message (irc-split-line message))
    (irc-privmsg *reply-target*
		 message)))
