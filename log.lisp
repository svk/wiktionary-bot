(in-package :wiktionary-bot)

(defvar *logging-lock* (mp:make-process-lock))

(defparameter *wiktionary-bot-logfile* #p"./logs/log.generated.lisp")

(defparameter *types-to-logfile* '(:info :warning :error))
(defparameter *types-to-irc* '(:info :warning :error))
(defparameter *types-to-stdout* '(:all))

(defmacro defun-log (name varlist &body body)
  `(defun ,name ,varlist
     (with-lock *logging-lock*
       ,@body)))

(defun-log matches-types? (type types)
  (or (find type types)
      (find :all types)))

(defun-log log-event (type origin format &rest values)
  (assert (and (symbolp origin)
	       (keywordp type)))
  (let* ((core-text (let ((*print-pretty* nil)) (apply #'format nil format values)))
	 (timestamp (get-universal-time))
	 (timestamp-string (format-timestamp timestamp))
	 (type-string (format nil "~a" type))
	 (origin-string (when origin
			  (format nil "~a" origin))))
    (when (matches-types? type *types-to-stdout*)
      (format t
	      "~a [~a~a~a] ~a~%"
	      timestamp-string
	      type-string
	      (if origin ":" "")
	      (or origin-string "")
	      core-text)
      (force-output))
    (when (matches-types? type *types-to-irc*)
      (irc-report (format nil
			  "[~a] ~a"
			  type-string
			  core-text)))
    (when (matches-types? type *types-to-logfile*)
      (sexp-to-file-appending 
       (list :type type
	     :timestamp timestamp
	     :readable-timestamp timestamp-string
	     :origin origin
	     :message core-text)
       *wiktionary-bot-logfile*))))

(defun-log log-info (origin format &rest values)
  (apply #'log-event :info origin format values))

(defun-log log-detail (origin format &rest values)
  (apply #'log-event :detail origin format values))

(defun-log log-test (origin format &rest values)
  (apply #'log-event :test origin format values))

(defun-log log-debug (origin format &rest values)
  (apply #'log-event :debug origin format values))

(defun-log log-warning (origin format &rest values)
  (apply #'log-event :warning origin format values))

(defun-log log-error (origin format &rest values)
  (apply #'log-event :error origin format values))

(defun-log log-and-signal-error (origin error-type &rest args)
  (log-event :error
	     origin
	     "signaling ~a (~a)"
	     error-type
	     args)
  (apply #'error
	 error-type
	 args))

