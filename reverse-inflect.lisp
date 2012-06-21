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
     :for (pattern replacement) :in (sexp-in-file *reverse-inflection-model*)
     :collect (list (cl-ppcre:create-scanner pattern)
		    replacement)))
			     
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



(defun reverse-inflect-with (inflected-word pairs)
  (remove-duplicates
   (loop
      :for (scanner replacement) :in pairs
      :if (cl-ppcre:scan scanner inflected-word)
      :collect (cl-ppcre:regex-replace scanner inflected-word replacement))
   :test #'equal))

(defun reverse-inflect (inflected-word)
  (reverse-inflect-with inflected-word *ri-regex-pairs*))

(defun missed-challenges (challenges)
  (loop
     :for (answer challenge) :in challenges
     :unless (find answer (reverse-inflect challenge) :test #'equal)
     :collect (cons challenge answer)))
     

(defun missed-ri-training ()
  (let* ((tr (ri-training-material))
	 (misses (missed-challenges tr)))
    (values misses
	    (coerce (/ (length misses) (length tr)) 'float))))

(defun ri-test ()
  (let ((testing-material (ri-testing-material)))
    (coerce (/ (length (missed-challenges testing-material))
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

(defun learn-reverse-inflections (challenges)
  (remove-if-not
   #'(lambda (pair)
       (let ((scanner (cl-ppcre:create-scanner (first pair))))
	 (> (length (remove-if-not
		     #'(lambda (a-c)
			 (equal (cl-ppcre:regex-replace scanner
							(second a-c)
							(second pair))
				(first a-c)))
		     challenges))
	    1)))
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
    :test #'equal)))


(defun learn-and-save-reverse-inflections ()
  (save-reverse-inflections (learn-reverse-inflections (ri-training-material))))

