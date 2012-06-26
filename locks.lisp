(in-package :wiktionary-bot)

(define-condition multiprocessing-deadlock (error)
  ((text :initarg :text :reader text)
   (lock-name :initarg :lock-name :reader multiprocessing-deadlock-lock-name)
   (timeout :initarg :timeout :reader multiprocessing-deadlock-timeout)))

(defmacro define-module-lock (lock-name with-name defun-name &key (timeout (* 5 60)))
  (list 'eval-when '(:compile-toplevel :load-toplevel :execute)
	`(defvar ,lock-name (mp:make-process-lock))
	(let ((sym (gensym "SYM")))
	  `(defmacro ,with-name (&body body)
	     (list 'let (list (list ',sym))
		   #+debug-locks
		   (list 'log-debug
			 '',with-name
			 "acquiring lock ~a"
			 '',lock-name)
		   (list 'block 'ok
			 (list 'mp:with-process-lock (list ',lock-name
							   :timeout
							   ,timeout)
			       (list 'return-from
				     'ok
				     (list 'setf
					   ',sym
					   (list* 'progn
						  body))))
			 (list 'error
			       ''multiprocessing-deadlock
			       :text "lock timeout"
			       :lock-name ',lock-name
			       :timeout ,timeout))
		   #+debug-locks
		   (list 'log-debug
			 '',with-name
			 "released lock ~a"
			 '',lock-name)
		   ',sym)))
	`(defmacro ,defun-name (name varlist &body body)
	   (list 'defun
		 name
		 varlist
		 #+debug-locks
		 (list 'log-debug
		       '',with-name
		       "in function ~a acquiring lock ~a"
		       (list 'quote name)
		       '',lock-name)
		 (list* ',with-name
			body)))))
