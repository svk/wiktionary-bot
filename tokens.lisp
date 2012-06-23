(in-package :wiktionary-bot)

(let ((regex (cl-ppcre:create-scanner "^(\\.+|\\!|\\?)$")))
  (defun sentence-ender? (word)
    (cl-ppcre:scan regex word)))

(defun capitalized? (word)
  (not (cl-unicode:has-property (char word 0)
				"LowercaseLetter")))

(defun known-abbreviation? (word)
  nil)

(defun tokens->sentence (tokens &key (key #'identity))
    (do* ((rest tokens)
	  (accepted nil)
	  (stop nil))
	((or (null rest)
	     stop) (values (reverse accepted)
			   rest))
      (let* ((current-raw (pop rest))
	     (current (funcall key current-raw))
	     (next (and (car rest) (funcall key (car rest))))
	     (last (and (car accepted) (funcall key (car accepted))))
	     (next-is-capitalized? (or (null rest)
				       (capitalized? next)))
	     (last-was-exception? (and (car accepted)
				       (known-abbreviation? last)))
	     (this-is-period? (sentence-ender? current)))
	(when (and this-is-period?
		   next-is-capitalized?
		   (not last-was-exception?))
	  (setf stop t))
	(push current-raw accepted))))


(defun tokens->sentences (tokens &key (key #'identity))
  (if (null tokens)
      nil
      (multiple-value-bind (sentence rest)
	  (tokens->sentence tokens :key key)
	(cons sentence (tokens->sentences rest :key key)))))

(defparameter *token-patterns* '(("\\w+" :TOKEN)
				 ("(:?\\w+[\\.'])+\\w+" :TOKEN)
				 ("((?:[a-z]+)://[\\w\\d:#@%/;$()~_?\+\\-=\\\.&]+)" :TOKEN)
				 ("(:?[0-9]+[ \\,\\.])*[0-9]+" :TOKEN)
				 ("\\s+" :WHITESPACE)
				 ("[\\.\\?\\(\\)\\[\\]\\:\\,\\;]" :TOKEN)
				 ("\\W" :TOKEN)))

(defun %argmax (f seq &optional acc acc-score)
  (if (null seq)
      acc
      (let* ((value (car seq))
	     (value-score (funcall f value)))
	(if (or (null acc-score)
		(> value-score acc-score))
	    (%argmax f (cdr seq) value value-score)
	    (%argmax f (cdr seq) acc acc-score)))))
      
(defun argmax (f seq)
  (%argmax f seq))

(let ((token-regexes (loop
			:for (pattern type)
			:in *token-patterns*
			:collect (list (cl-ppcre:create-scanner (concatenate 'string "^" pattern))
				       type))))
  (defun first-token (text &key (start 0) offsets)
    (destructuring-bind (end . type)
	(argmax #'car
		(mapcar #'(lambda (scanner-type)
			    (multiple-value-bind (begin end)
				(cl-ppcre:scan (first scanner-type) text :start start)
			      (if (null begin)
				  (cons 0 nil)
				  (cons end (second scanner-type)))))
			token-regexes))
      (values (if offsets
		  (cons start end)
		  (subseq text start end))
	      type
	      (- end start)))))

(defun tokenize (text &key (types '(:token)) offsets)
  (let ((text-length (length text))
	(index 0))
    (collecting 
      (loop
	 :until (>= index text-length)
	 :do (multiple-value-bind (token type token-length)
		 (first-token text :start index :offsets offsets)
	       (incf index token-length)
	       (when (find type types)
		 (collect token)))))))

(let ((boundary-regex (cl-ppcre:create-scanner "\\b")))
  (defun tokenize-text (text)
    (remove-if (papply (eql ? 0))
	       (mapcar #'trim
		       (cl-ppcre:split boundary-regex text))
	       :key #'length)))
