(in-package :wiktionary-bot)

(defparameter *wiki-template-regex* (cl-ppcre:create-scanner "{{.*?}}"))
(defparameter *wiki-table-regex* (cl-ppcre:create-scanner "{\\|.*?\\|}"))
(defparameter *wiki-link-regex* (cl-ppcre:create-scanner "\\[\\[([^|]+?\\|)?([^|]+?)\\]\\]"))
(defparameter *wiki-piped-link-regex* (cl-ppcre:create-scanner "\\[\\[([^\\|]+?)\\|([^\\]]+?)\\]\\]"))
(defparameter *wiki-unpiped-link-regex* (cl-ppcre:create-scanner "\\[\\[([^\\|]+?)\\]\\]"))
(defparameter *wiki-external-link-regex* (cl-ppcre:create-scanner "(^|[^\\[])\\[([^\\[ ]+?)(?: ([^\\]]+))?\\]"))
(defparameter *wiki-named-external-link-regex* (cl-ppcre:create-scanner "(^|[^\\[])\\[([^\\[ ]+?)(?: ([^\\]]+))\\]"))
(defparameter *wiki-unnamed-external-link-regex* (cl-ppcre:create-scanner "(^|[^\\[])\\[([^\\[]+?)\\]"))
(defparameter *wiki-comment-regex* (cl-ppcre:create-scanner "<!--.*?-->"))

(defun strip-wiki-markup (text)
  (setf text (cl-ppcre:regex-replace-all *wiki-comment-regex* text ""))
  (setf text (cl-ppcre:regex-replace-all *wiki-template-regex* text ""))
  (setf text (cl-ppcre:regex-replace-all *wiki-table-regex* text ""))
  (setf text (cl-ppcre:regex-replace-all *wiki-piped-link-regex* text "\\2"))
  (setf text (cl-ppcre:regex-replace-all *wiki-unpiped-link-regex* text "\\1"))
  (setf text (cl-ppcre:regex-replace-all *wiki-named-external-link-regex* text "\\1\\3"))
  (setf text (cl-ppcre:regex-replace-all *wiki-unnamed-external-link-regex* text "\\1\\2"))
  text)

(defparameter *url-regex* "((?:https?|ftp)://[\\w\\d:#@%/;$()~_?\+-=\\\.&]+)")

(defparameter *wiki-tokens*
  '(("{{" {{)
    ("}}" }})
    ("((?:https?|ftp)://[\\w\\d:#@%/;$()~_?\+\\-=\\\.&]+)" :URL)
;;    ("((?:https?|ftp)://[\\w\\d/\\.\\-=]+)" :URL)
    ("\\n\\*\\*\\*\\*" ****)
    ("\\n\\*\\*\\*" ***)
    ("\\n\\*\\*" **)
    ("\\n\\*" *)
    ("\\n" newline)
    ("</" </)
    ("<" <)
    (">" >)
    ("{\\|" {!)
    ("\\|}" !})
    ("----" ----)
    ("\\|" !)
    ("\\[\\[" [[)
    ("\\]\\]" ]])
    ("\\[" [)
    ("\\]" ])
    ("'''''" |'''''|)
    ("'''" |'''|)
    ("''" |''|)
    ("====" ====)
    ("===" ===)
    ("==" ==)
    ("=" =)))

(def-simple-cached compiled-wiki-tokens
  (mapcar #'(lambda (regex-symbol)
			       (destructuring-bind (regex symbol)
				   regex-symbol
				 (list (cl-ppcre:create-scanner (concatenate 'string "^" regex))
				       (cl-ppcre:create-scanner regex)
				       symbol)))
	  *wiki-tokens*))


(defun create-wiki-lexer (text)
  (let ((position 0))
    #'(lambda ()
	(block return-token
	  (when (>= position (length text))
	    (return-from return-token (values nil nil)))
	  (destructuring-dolist ((regex search-regex token) (compiled-wiki-tokens))
	    (declare (ignore search-regex))
	    (multiple-value-bind (start end starts ends)
		(cl-ppcre:scan regex text :start position)
	      (when start
		(setf position end)
		(if (> (length starts) 0)
		    (return-from return-token (values token (subseq text (aref starts 0) (aref ends 0))))
		    (return-from return-token (values token token))))))
	  (let* ((matches (remove-if-not #'identity
					 (mapcar (compose (papply (cl-ppcre:scan ? text :start position))
							  #'second)
						 (compiled-wiki-tokens))))
		 (end (if matches
			  (reduce #'min matches)
			  (length text)))
		 (rv (subseq text position end)))
	    (setf position end)
	    (values 'string rv))))))

(defun lexer->list (lexer)
  (collecting
    (loop
       :do (multiple-value-bind (type value)
	       (funcall lexer)
	     (unless (or type value)
	       (return))
	     (collect value)))))

(defun make-wikilink (page-name &rest args)
  `(wikilink ,page-name
	     ,@args))

(defun make-tag (content &key close)
  (let* ((seq (split-sequence #\space content))
	 (tag-name (intern (string-upcase (car seq))))
	 (attrs (cdr seq)))
    (list (if close
	      'close-tag
	      'open-tag)
	  tag-name
	  attrs)))

(lispbuilder-yacc:define-parser *wiki-parser*
  (:start-symbol text+)
  (:terminals (string [[ ! ]] {{ }} |''| |'''| ---- = == === ==== [ ] < > </ newline * ** *** **** :url |'''''| {! !}))
  (:precedence ((:left == === ====)))

  (raw-string-element
   string
   (newline #'(lambda (x) (coerce (list #\newline) 'string)))
   (= #'(lambda (x) "=")))

  (raw-string
   raw-string-element
   (raw-string raw-string-element #'(lambda (a b) (concatenate 'string a b))))


  (text+
   (text #'list)
   (text text+ #'cons))

  (text*
   (nil #'list)
   (text #'list)
   (text text+ #'cons))

  (non-headline-text+
   (non-headline-text #'list)
   (non-headline-text non-headline-text+ #'cons))

  (non-italic-text+
   (non-italic-text #'list)
   (non-italic-text non-italic-text+ #'cons))

  (non-bolditalic-text+
   (non-bolditalic-text #'list)
   (non-bolditalic-text non-bolditalic-text+ #'cons))

  (non-bold-text+
   (non-bold-text #'list)
   (non-bold-text non-bold-text+ #'cons))
  
  (piped-text+-list
   (text+ #'list)
   (text+ ! piped-text+-list #'(lambda ( a b c) (cons a c))))

  (piped-text*-list
   (text* #'list)
   (text* ! piped-text*-list #'(lambda ( a b c) (cons a c))))

  (piped-string-list
   (string #'list)
   (string ! piped-string-list #'(lambda (a b c) (cons a c))))


  (text
   ({{ template-contents }} #'(lambda ({{ ls }}) `(template ,@ls)))
   ({! table-contents !} #'(lambda ({{ ls }}) `(table ,@ls)))
   (== non-headline-text+ == #'(lambda (a b c) `(header 1 ,b)))
   (=== non-headline-text+ === #'(lambda (a b c) `(header 2 ,b)))
   (==== non-headline-text+ ==== #'(lambda (a b c) `(header 3 ,b)))
   (* #'(lambda (x) `(list-element 1)))
   (** #'(lambda (x) `(list-element 2)))
   (*** #'(lambda (x) `(list-element 3)))
   (**** #'(lambda (x) `(list-element 4)))
   non-headline-text)

  (non-headline-text
   (|'''''| non-bolditalic-text+ |'''''| #'(lambda (o x c) `(bold (italic ,x))))
   (|''| non-italic-text+ |''| #'(lambda (o x c) `(italic ,x)))
   (|'''| non-bold-text+ |'''| #'(lambda (o x c) `(bold ,x)))
   non-bolditalic-text)

  (non-italic-text
   (|'''| non-bolditalic-text+ |'''| #'(lambda (o x c) `(italic ,x)))
   non-bolditalic-text)

  (non-bold-text
   (|''| non-bolditalic-text+ |''| #'(lambda (o x c) `(bold ,x)))
   non-bolditalic-text)

  (non-bolditalic-text
   ([[ piped-text+-list ]] #'(lambda ([[ x ]]) (apply #'make-wikilink x)))
   ([ :url text+ ] #'(lambda ([ a b ]) `(external-link :url ,a  :desc ,b)))
   ([ :url ] #'(lambda ([ a ]) `(external-link :url ,a :desc ,a)))
   (---- (lambda (x) `(horizontal-bar)))
   (< raw-string > #'(lambda (a b c) (funcall #'make-tag b)))
   (</ raw-string > #'(lambda (a b c) (funcall #'make-tag b :close t)))
   :url
   string
   (newline #'(lambda (x) (coerce (list #\newline) 'string)))
   (= #'(lambda (x) "=")))

  (text+-or-key-value
   nil
   text
   (string = text+ #'(lambda (a b c) (cons a c))))

  (table-contents
   piped-text+-list)
  
  (template-contents
   piped-text*-list))

(defun merge-all-adjacent-strings (structure)
  (mapcar #'(lambda (x) (if (listp x)
			    (merge-all-adjacent-strings x)
			    x))
	  (merge-adjacent-strings structure)))


(defun lex-wikitext (wikitext)
  (lexer->list (create-wiki-lexer wikitext)))

(defun parse-wikitext (wikitext)
  (lispbuilder-yacc:parse-with-lexer (create-wiki-lexer wikitext) *wiki-parser*))

(defun parse-wikitext-ignore-errors (wikitext)
  (handler-case 
      (parse-wikitext wikitext)
    (lispbuilder-yacc:yacc-parse-error nil)))


(defun wikilink->text (wikilink)
  (nth (1- (length wikilink)) wikilink))

(defun collect-atoms (structure)
  (if (consp structure)
      (nconc (collect-atoms (car structure))
	     (collect-atoms (cdr structure)))
      (unless (null structure)
	(list structure))))

(defparameter *namespaced-name-regex* (cl-ppcre:create-scanner "([^:]+):(.+)"))

(define-condition wikitext-extraction-error (error)
  ((text :initarg :text :reader text)))


(defun filter-ignored (list &key (begin :BEGINIGNORE) (end :ENDIGNORE))
  (let ((ignoring 0))
    (loop
       :for element :in list
       :if (eq element begin)
       :do (incf ignoring)
       :if (eq element end)
       :do (setf ignoring (max 0 (decf ignoring)))
       :if (and (not (or (eq element begin)
		    (eq element end)))
		(zerop ignoring))
       :collect element)))

       
  

(defun %parsed-wikitext->text (wt)
  (when (keywordp wt)
    (return-from %parsed-wikitext->text wt))
  (funcall #'merge-adjacent-strings
	 (collect-atoms
	  (cond ((consp wt)
		 (destructuring-bind (type . args)
		     wt
		   (if (symbolp type)
		       (%parsed-wikitext->text (case type
						 (template nil)
						 (table nil)
						 (wikilink
						  (let ((text (parsed-wikitext->text (wikilink->text wt))))
						    (multiple-value-bind (match)
							(cl-ppcre:scan-to-strings *namespaced-name-regex*
										  text)
						      (if (not match)
							  text
							  nil))))
						 (bold args)
						 (italic args)
						 (list-element ".")
						 (header ".")
						 (external-link nil)
						 (open-tag (if (eq (car args) 'REF)
							       :BEGINIGNORE
							       nil))
						 (close-tag (if (eq (car args) 'REF)
								:ENDIGNORE
								nil))
						 (horizontal-bar ".")
						 (otherwise (error 'wikitext-extraction-error
								   :text (format nil "unexpected symbol ~a" type)))))
		       (merge-adjacent-strings
			(remove-if-not #'identity
				       (mapcar #'%parsed-wikitext->text wt))))))
		(t wt)))))


(let ((nbsp-regex (cl-ppcre:create-scanner "&nbsp;"))
      (whitespace-regex (cl-ppcre:create-scanner "\\s+")))
  (defun parsed-wikitext->text (wt)
    (setf wt (apply #'concatenate 'string (filter-ignored (%parsed-wikitext->text wt))))
    (setf wt (cl-ppcre:regex-replace-all nbsp-regex wt " "))
    (setf wt (cl-ppcre:regex-replace-all whitespace-regex wt " "))
    (trim wt)))


(let ((boundary-regex (cl-ppcre:create-scanner "\\b")))
  (defun parsed-wikitext->tokens (wt)
    (remove-if (papply (eql ? 0))
	       (mapcar #'trim
		       (cl-ppcre:split boundary-regex (parsed-wikitext->text wt)))
	       :key #'length)))

(defun all-matches (regex text)
		  (do ((regex (if (stringp regex)
				  (cl-ppcre:create-scanner regex)
				  regex))
		       (rv)
		       (index 0)
		       (text-length (length text)))
		      ((>= index text-length)
		       (reverse rv))
		    (multiple-value-bind (begin end)
			(cl-ppcre:scan regex text :start index)
		      (when begin
			(push (list begin end) rv))
		      (setf index (or end
				      text-length)))))

(let ((regex (cl-ppcre:create-scanner "(?=[^\\n])=+[^=]+=+(?=\\s*\\n)"))
      (headline-regex (cl-ppcre:create-scanner "^(=+)([^=]+)(=+)$")))
  (defun find-all-headlines-flat (wikitext)
    (remove-if-not #'identity
		   (mapcar #'(lambda (indices)
			       (let ((string (trim (apply #'subseq wikitext indices))))
				 (multiple-value-bind (full-match groups)
				     (cl-ppcre:scan-to-strings headline-regex string)
				   (when (and full-match
					      (eql (length (aref groups 0))
						   (length (aref groups 2))))
				     (append indices
					     (list (aref groups 1)
						   (length (aref groups 0))))))))
			   (all-matches regex wikitext)))))

(defun get-direct-contents (wikitext matches)
  (when matches
    (cons (append (car matches)
		  (list (if (null (cdr matches))
			    (subseq wikitext (second (car matches)))
			  (subseq wikitext (second (car matches)) (first (cadr matches))))
			(second (car matches))
			(if (null (cdr matches))
			    (length wikitext)
			    (first (cadr matches)))))
	  (get-direct-contents wikitext (cdr matches)))))

(defun make-dummy-headline (level)
  (list nil nil
	nil
	level
	nil))

(defun get-direct-contents+intro (wikitext matches)
  (cons (list nil nil
	      nil
	      0
	      (subseq wikitext 0 (if matches
				     (caar matches)
				     nil)))
	(get-direct-contents wikitext matches)))

(defun interpose-dummy-headlines! (list)
  (if (< (length list) 2)
      list
      (do ((alpha list)
	   (beta (cdr list))
	   (rest (cddr list)))
	  ((null beta) list)
	(if (> (headline-entry-level (car beta)) (1+ (headline-entry-level (car alpha))))
	    (setf alpha
		  (setf (cdr alpha)
			(cons (make-dummy-headline (1+ (headline-entry-level (car alpha)))) beta)))
	    (setf alpha beta
		  beta rest
		  rest (cdr rest))))))

(defun headline-entry-name (x) (third x))
(defun headline-entry-level (x) (fourth x))
(defun headline-entry-direct-contents (x) (fifth x))
 
(defun take-until (predicate list)
  (let ((selected (collecting
		    (do ((rest list (cdr rest)))
			((or (null rest)
			     (funcall predicate (car rest)))
			 (setf list rest))
		      (collect (car rest))))))
    (values selected list)))

(defun negation-of-predicate (predicate)
  #'(lambda (&rest args) (not (apply predicate args))))

(defun take-while (predicate list)
  (take-until (negation-of-predicate predicate)
	      list))

(defun split-sequence-at-heads (predicate list &optional acc)
  (if (null list)
      (reverse (mapcar #'reverse acc))
      (split-sequence-at-heads predicate
			       (cdr list)
			       (if (funcall predicate (car list))
				   (cons (cons (car list) nil) acc)
				   (progn (push (car list) (car acc))
					  acc)))))

(defun unflatten-headlines (list)
  (destructuring-bind (head . children)
      list
    (nconc (list (headline-entry-name head))
	   (unless (or (not (headline-entry-direct-contents head))
		       (equal "" (headline-entry-direct-contents head)))
	     (list (headline-entry-direct-contents head)))
	   (remove-if-not #'identity
			  (mapcar #'unflatten-headlines
				  (split-sequence-at-heads #'(lambda (entry)
							       (= (1+ (headline-entry-level head))
								  (headline-entry-level entry)))
							   children))))))

(defun structure-wikitext (wikitext)
  (unflatten-headlines (interpose-dummy-headlines! (get-direct-contents+intro wikitext (find-all-headlines-flat wikitext)))))

(defun headlines-from-structure (structure)
  (cons (car structure)
	(mapcar #'headlines-from-structure (remove-if-not #'consp (cdr structure)))))

(defun headline-structure (wikitext)
  (headlines-from-structure (structure-wikitext wikitext)))


(defun %select-section (section-specifier structure)
  (if (null section-specifier)
      (values structure t)
      (%select-section
       (cdr section-specifier)
       (or (find-if #'(lambda (key) (equal key (car section-specifier)))
		    (remove-if-not #'consp (cdr structure))
		    :key #'car)
	   (return-from %select-section (values nil nil))))))

(defun select-section (section-specifier structure)
  (multiple-value-bind (elements present-p)
      (%select-section section-specifier structure)
    (values (cdr elements) present-p)))

(defun select-section-text (section-specifier structure)
  (multiple-value-bind (elements present-p)
      (select-section section-specifier structure)
    (if present-p
	(values (first (merge-adjacent-strings elements)) t)
	(values "" nil))))

      