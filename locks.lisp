(in-package :wiktionary-bot)

(defmacro define-module-lock (lock-name with-name defun-name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defvar ,lock-name (mp:make-process-lock))
     (defmacro ,with-name (&body body)
       (list* 'mp:with-process-lock
	     (list ,lock-name)
	     body))
     (defmacro ,defun-name (name varlist &body body)
       (list* 'defun
	      name
	      varlist
	      (list* ,lock-name body)))))
