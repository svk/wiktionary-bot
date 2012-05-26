(in-package :wiktionary-bot)

(let ((regex (cl-ppcre:create-scanner "^(\\.+|\\!|\\?)$")))
  (defun sentence-ender? (word)
    (cl-ppcre:scan regex word)))

(defun capitalized? (word)
  (not (cl-unicode:has-property (char word 0)
				"LowercaseLetter")))

(defun known-abbreviation? (word)
  nil)

(defun tokens->sentence (tokens)
    (do* ((rest tokens)
	  (accepted nil)
	  (stop nil))
	((or (null rest)
	     stop) (values (reverse accepted)
			   rest))
      (let* ((current (pop rest))
	     (next-is-capitalized? (or (null rest)
				       (capitalized? (car rest))))
	     (last-was-exception? (and (car accepted)
				       (known-abbreviation? (car accepted))))
	     (this-is-period? (sentence-ender? current)))
	(when (and this-is-period?
		   next-is-capitalized?
		   (not last-was-exception?))
	  (setf stop t))
	(push current accepted))))


(defun tokens->sentences (tokens)
  (if (null tokens)
      nil
      (multiple-value-bind (sentence rest)
	  (tokens->sentence tokens)
	(cons sentence (tokens->sentences rest)))))
