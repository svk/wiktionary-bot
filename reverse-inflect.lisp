(in-package :wiktionary-bot)

(defparameter *reverse-inflection-training-material* #p"./data/ri-training.generated.lisp")
(defparameter *reverse-inflection-testing-material* #p"./data/ri-testing.generated.lisp")

(defparameter *reverse-inflection-model* #p"./data/ri-model.generated.lisp")

(defun ri-training-material ()
  (remove-if
   #'(lambda (x) (find x (ri-testing-material) :test #'equal))
   (list-in-file *reverse-inflection-training-material*)))

(defun ri-testing-material ()
  (list-in-file *reverse-inflection-testing-material*))

(defparameter *reverse-inflection-testing-current* (length (ri-testing-material)))
(defparameter *reverse-inflection-testing-goal* 1000)


(defun load-reverse-inflections ()
  (loop
     :for (pattern replacement correct applies) :in (sexp-in-file *reverse-inflection-model*)
     :collect (list (cl-ppcre:create-scanner pattern)
		    replacement
		    (/ correct applies))))
			     
(defparameter *ri-regex-pairs* (load-reverse-inflections))

(defmacro def-reverse-inflection (inflected-word-regex replacement-to-base)
  `(push (cons (cl-ppcre:create-scanner ,inflected-word-regex) ,replacement-to-base)
	 *ri-regex-pairs*))

(defun add-to-reverse-inflect-material (base-word inflected-word)
  (let ((testing))
    (when (and (< *reverse-inflection-testing-current*
		  *reverse-inflection-testing-goal*)
	       (< (random 1.0) 0.5))
      (incf *reverse-inflection-testing-current*)
      (setf testing t))
    (with-open-file (*standard-output* (if testing
					   *reverse-inflection-testing-material*
					   *reverse-inflection-training-material*)
				       :direction :output :if-exists :append :if-does-not-exist :create)
      (write (list base-word inflected-word)))))



(defun reverse-inflect-with-weights (inflected-word pairs)
  (sort (let ((ht (make-hash-table :test #'equal)))
	  (loop
	     :for (scanner replacement weight) :in pairs
	     :if (cl-ppcre:scan scanner inflected-word)
	     :do (incf (gethash (cl-ppcre:regex-replace scanner inflected-word replacement) ht 0)
		       weight))
	  (maphash-to-unordered-list #'cons ht))
	#'>
	:key #'cdr))

(defun reverse-inflect-with (inflected-word pairs)
  (mapcar #'car (reverse-inflect-with-weights inflected-word pairs)))

(defun reverse-inflect (inflected-word)
  (reverse-inflect-with inflected-word *ri-regex-pairs*))

(defun missed-challenges (challenges &key limit)
  (loop
     :for (answer challenge) :in challenges
     :unless (find answer (funcall (if limit
				       (papply (first-n ? limit))
				       #'identity)
				   (reverse-inflect challenge))
		   :test #'equal)
     :collect (cons challenge answer)))
     

(defun missed-ri-training (&optional limit)
  (let* ((tr (ri-training-material))
	 (misses (missed-challenges tr :limit limit)))
    (values misses
	    (coerce (/ (length misses) (length tr)) 'float))))

(defun ri-test (&optional limit)
  (let ((testing-material (ri-testing-material)))
    (coerce (/ (length (missed-challenges testing-material :limit limit))
	       (length testing-material))
	    'float)))

(defun prefix (string n)
  (if (> n (length string))
      string
      (subseq string 0 n)))

(defun suffix (string n)
  (let ((l (length string)))
    (if (> n l)
	string
	(subseq string (- l n)))))

(defun longest-common-prefix (string &rest strings)
  (do* ((all-strings (cons string strings))
	(limit (reduce #'min strings :key #'length :initial-value (length string)))
	(current-length 0 (1+ current-length))
	(best "" candidate)
	(candidate (prefix string current-length) (prefix string current-length)))
      ((or (> current-length limit)
	   (some #'(lambda (x) (not (equal (subseq x 0 (length candidate))
					   candidate)))
		 all-strings))
       best)))

(defun distinguishing-suffixes (&rest strings)
  (let ((cutoff (length (apply #'longest-common-prefix strings))))
    (mapcar #'(lambda (x) (subseq x cutoff))
	    strings)))

(defun save-reverse-inflections (pairs)
  (with-open-file (*standard-output* *reverse-inflection-model* :direction :output :if-exists :supersede)
    (write pairs)))

;; P(h|c)P(c) = P(h,c) = P(c|h)P(h)
;; bayes P(c|h) = P(h|c)P(c)/P(h)
;; however P(not-h|c)=0 , P(h,c) = P(c)
;; P(c|h) = P(c)/P(h)

(defun learn-reverse-inflections (challenges)
  (remove-if-not
   #'(lambda (x) (> x 1))
   (mapcar
    #'(lambda (pair)
	(let* ((scanner (cl-ppcre:create-scanner (first pair)))
	       (correct (length (remove-if-not
				 #'(lambda (a-c)
				     (equal (cl-ppcre:regex-replace scanner
								    (second a-c)
								    (second pair))
					    (first a-c)))
				 challenges)))
	       (applies (length (remove-if-not
				 #'(lambda (a-c) (cl-ppcre:scan scanner (second a-c)))
				 challenges))))
	  (append pair (list correct applies))))
    (remove-duplicates
     (loop
	:for (answer challenge) :in challenges
	:collect (destructuring-bind (base-suffix inflection-suffix)
		     (distinguishing-suffixes answer challenge)
		   (list (concatenate 'string
				      "^(.+)"
				      inflection-suffix
				      "$")
			 (if (equal base-suffix "")
			     "\\1"
			     (concatenate 'string
					  "\\1"
					  base-suffix)))))
     :test #'equal))
   :key #'third))


(defun learn-and-save-reverse-inflections ()
  (save-reverse-inflections (learn-reverse-inflections (ri-training-material))))

(defun reverse-inflect-dumps (inflected-word)
  (remove-if-not #'swedish-dump-text
	     (reverse-inflect inflected-word)))

(defun reverse-inflect-network (inflected-word &key (limit 5) (use-dumps t))
  (let ((candidates (reverse-inflect inflected-word)))
    (unless (or (not use-dumps)
		(<= (length candidates) 1))
      (let ((new-candidates (remove-if-not #'swedish-dump-text candidates)))
	(when new-candidates
	  (setf candidates new-candidates))))
    (setf candidates  (funcall (if limit
				 (papply (first-n ? limit))
				 #'identity)
			       candidates))
    (unless (<= (length candidates) 1)
      (let ((present (mapcar #'(lambda (x) (unless (assoc :missing (cdr x)) (extract '(:title) (cdr x))))
			     (extract '(:query :pages) (api-query :query :prop
								  :prop :info
								  :titles candidates)))))
	(setf candidates (delete-if-not (papply (find ? present :test #'equal))
					candidates))))
    (dolist (candidate candidates)
      (when (find inflected-word
		  (links-from-table-cell (scan-for-grammar-tables (swedish-blessed-grammar-templates)
								  candidate
								  :early-warning nil
								  :fully-live t))
		  :test #'equal)
	(return-from reverse-inflect-network candidate)))
    (values nil
	    (car (reverse-inflect inflected-word)))))