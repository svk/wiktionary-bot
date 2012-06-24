(in-package :wiktionary-bot)

(defun matcher? (item)
  (and (consp item) (eq (car item) :matcher)))

(defun make-aos-matcher (item)
  (if (matcher? item)
      item
      (cons :matcher
	    (labels ((match-any (things predicate)
		       (if (consp things)
			   (find-if predicate things)
			   (funcall predicate things))))
	      (macrolet ((m-papply (form)
			   `#'(lambda (things) (match-any things (papply ,form)))))
		(cond ((stringp item) (m-papply (equal item ?)))
		      ((eq item t) #'identity)
		      ((symbolp item) (m-papply (eq item ?)))
		      ((functionp item)
		       #'(lambda (things) (match-any things item)))
		      ((consp item)
		       (destructuring-bind (type . contents)
			   item
			 (case type
			   (regex (destructuring-bind (pattern)
				      contents
				    (let ((scanner (if (functionp pattern)
						       pattern
						       (cl-ppcre:create-scanner pattern))))
				      (m-papply (cl-ppcre:scan scanner ?)))))
			   (otherwise
			    (let ((matchers (mapcar #'make-aos-matcher item)))
			      #'(lambda (things)
				  (apply-aos-matchers matchers things)))))))))))))

(defun apply-aos-matcher (item things)
  (funcall (cdr (make-aos-matcher item)) things))

(defun apply-aos-matchers (items things)
  (dolist (item items)
    (unless (apply-aos-matcher item things)
      (return-from apply-aos-matchers)))
  t)

(defun preprocess-aos-matcher-arguments (arguments)
  (loop
     :for (name value)
     :in (bunch arguments 2)
     :nconc (list name (make-aos-matcher value))))

(defun lhtml-attribute (tag name &optional default)
  (or (cadr (assoc name (cadr tag)))
      default))

(defun lhtml-matches? (tag &rest arguments &key &allow-other-keys)
  (destructuring-dolist ((name value) (bunch arguments 2))
    (let ((matcher (make-aos-matcher value)))
      (unless (case name
		(:name (apply-aos-matcher matcher (car tag)))
		(:class (apply-aos-matcher matcher
					   (split-sequence #\space (cadr (assoc :class (cadr tag))))))
		(otherwise (apply-aos-matcher matcher (cadr (assoc name (cadr tag))))))
	(return-from lhtml-matches?))))
  t)

(defun lhtml-matches?-old (tag &key name id class)
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

(defun %lhtml-filter (root &rest arguments &key &allow-other-keys)
  (if (or (null root)
	  (stringp root))
      root
      (destructuring-bind (tag-name attrs . contents)
	  root
	(list* tag-name
	       attrs
	       (merge-adjacent-strings
		(mapcar #'(lambda (x)
			    (apply #'%lhtml-filter
				   x
				   arguments))
			(remove-if #'(lambda (element)
				       (and (not (stringp element))
					    (apply #'lhtml-matches?
						   element
						   arguments)))
				   contents)))))))

(defun lhtml-filter (root &rest arguments &key &allow-other-keys)
  (apply #'%lhtml-filter
	 root
	 (preprocess-aos-matcher-arguments arguments)))

(defun %lhtml-select (root &rest arguments &key &allow-other-keys)
  (if (or (null root)
	  (stringp root))
      nil
      (do* ((stack (list root))
	    (current (pop stack) (pop stack))
	    (matching))
	   ((null current) (reverse matching))
	(when (consp current)
	  (when (apply #'lhtml-matches?
		       current
		       arguments)
	    (return-from %lhtml-select current)
	    (push current matching))
	  (dolist (tag (cddr current))
	    (push tag stack))))))

(defun %lhtml-select-list (root &rest arguments &key &allow-other-keys)
  (if (or (null root)
	  (stringp root))
      nil
      (do* ((stack (list root))
	    (current (pop stack) (pop stack))
	    (matching))
	   ((null current) (reverse matching))
	(when (consp current)
	  (when (apply #'lhtml-matches?
		       current
		       arguments)
	    (push current matching))
	  (dolist (tag (cddr current))
	    (push tag stack))))))

(defun lhtml-select-list (root &rest arguments &key &allow-other-keys)
  (apply #'%lhtml-select-list
	 root
	 (preprocess-aos-matcher-arguments arguments)))


(defun lhtml-select-single (root &rest arguments &key &allow-other-keys)
  (apply #'%lhtml-select
	 root
	 (preprocess-aos-matcher-arguments arguments)))

(defun lhtml-select (root &rest arguments &key list &allow-other-keys)
  (let ((arguments (flatten (remove-if (papply (eq ? :list))
				       (bunch arguments 2)
				       :key #'car))))
    (apply (if list
	       #'lhtml-select-list 
	       #'lhtml-select-single)
	   root
	   arguments)))

(defmacro %call-sequence-1st-reversed (target call &rest calls)
  `(,(car call)
     ,(if (null calls)
	  target
	  (list* '%call-sequence-1st-reversed
		 target
		 calls))
     ,@(cdr call)))

(defmacro call-sequence-1st (target &rest calls)
  `(%call-sequence-1st-reversed ,target
				,@(reverse calls)))

(defmacro call-sequence (target &rest calls)
  `(call-sequence-1st ,target ,@calls))
