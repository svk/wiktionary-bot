(in-package :wiktionary-bot)

(defun word-in-swedish-wiktionary? (word)
  (gethash (string-upcase word) (swedish-title-table)))

(defvar *unknown-words* (make-hash-table :test #'equal))

(defun clear-unknown-words ()
  (setf *unknown-words* (make-hash-table :test #'equal)))

(defun show-unknown-words ()
  (destructuring-dolist ((word sentence title) (hash-table-values *unknown-words*))
    (format t "~a: [~a] ~a~%" word title sentence)))

(defun unknown-words ()
  (collecting
    (destructuring-dolist ((word sentence title) (hash-table-values *unknown-words*))
      (declare (ignore sentence title))
      (collect word))))

(defun is-inflection? (word)
  (reverse-inflect-dumps word))

(let ((number-regex (cl-ppcre:create-scanner "\\d+"))
      (wordlike-regex (cl-ppcre:create-scanner "^[a-zäöå\\-]+$")))
  (defun uninteresting-word? (word)
    (or (cl-ppcre:scan number-regex word)
	(not (cl-ppcre:scan wordlike-regex word))
	(is-autowikibrowser-typo? word)
	(is-inflection? word)
	(capitalized? word))))


(defparameter *unknown-word-sentence-taboos* (mapcar #'cl-ppcre:create-scanner
						     '("http")))

(defun scan-sentence-for-unknown-words (sentence &key (key #'identity))
  (loop
     :for word :in sentence
     :if (let ((real-word (funcall key word)))
	   (not (or (word-in-swedish-wiktionary? real-word)
		    (uninteresting-word? real-word))))
     :collect word))

(let ((redirect-regex (cl-ppcre:create-scanner "#redirect" :case-insensitive-mode t))
      (robot-regex (cl-ppcre:create-scanner "{{robotskapad" :case-insensitive-mode t)))
  (defun scan-for-unknown-words (title)
    (when (listp title)
      (loop
	 :for actual-title :in title
	 :for i :from 1
	 :do (scan-for-unknown-words actual-title)
	 :do (when (zerop (mod i 1000))
	       (format t
		       "[scan-for-unknown-words] ~a: processed ~a (word #~a)~%"
		       (rfc3339:make-timestamp)
		       actual-title
		       i)))
      (return-from scan-for-unknown-words))
    (handler-case 
	(labels ((exclude-word (word)
		   (or (word-in-swedish-wiktionary? word)
		       (uninteresting-word? word)))
		 (exclude-sentence (sentence)
		   (or (< (length sentence) 3)
		       (loop
			  :for taboo :in *unknown-word-sentence-taboos*
			  :if (some (papply (cl-ppcre:scan taboo ?)) sentence)
			  :collect t)
		       (> (length (remove-if #'exclude-word
					     sentence))
			  1)))
		 (exclude-article (raw-text)
		   (or (cl-ppcre:scan redirect-regex raw-text)
		       (cl-ppcre:scan robot-regex raw-text))))
	  (let ((raw-text (swedish-wp-text title)))
	    (unless (exclude-article raw-text)
	      (let ((wikitext (parse-wikitext-ignore-errors raw-text)))
		(when wikitext
		  (let ((sentences (tokens->sentences (parsed-wikitext->tokens wikitext))))
		    (loop :for sentence :in sentences
		       :unless (exclude-sentence sentence)
		       :do (loop :for word :in sentence
			      :unless (exclude-word word)
			      :do (let ((entry (gethash (string-upcase word) *unknown-words* nil)))
				    (if entry
					(pushnew title (third entry) :test #'equal)
					(setf (gethash (string-upcase word) *unknown-words*)
					      (list word sentence (list title)))))))))))))
      (wikitext-extraction-error () nil))))

(def-simple-cached autowikibrowser-typos
  (let ((regex (cl-ppcre:create-scanner "<Typo\\s+word=\"([^\"]+)\"\\s+find=\"([^\"]+)\"\\s+replace=\"([^\"]+)\"\\s+/>"))
	(position 0)
	(text (wikipedia-page-source "Wikipedia:AutoWikiBrowser/Typos")))
    (cdr (collecting
	   (loop
	      :do (multiple-value-bind (begin end)
		      (cl-ppcre:scan regex text :start position)
		    (if (not begin)
			(return)
			(progn (multiple-value-bind (match strings)
				   (cl-ppcre:scan-to-strings regex text :start position)
				 (declare (ignore match))
				 (handler-case (collect (list (aref strings 0)
							      (cl-ppcre:create-scanner (aref strings 1))
							      (cl-ppcre:regex-replace-all
							       "\\$(\\d+)"
							       (aref strings 2)
							       "\\\\\\1")))
				   (cl-ppcre:ppcre-syntax-error ()
				     (format t "[autowikibrowser-typos] unable to parse ~a (~a -> ~a)~%" (aref strings 0) (aref strings 1) (aref strings 2)))))
			       (setf position end)))))))))

(defun is-autowikibrowser-typo? (word)
  (loop
     :for (pattern regex replacement) :in (autowikibrowser-typos)
     :if (equal word (cl-ppcre:scan-to-strings regex word))
     :do (let ((replacement-word (cl-ppcre:regex-replace regex word replacement)))
	   (return-from is-autowikibrowser-typo? (and (not (equal replacement-word word))
						      replacement-word))))
  nil)

(defun best-unknown-words (&key (n 100) skip)
  (single-value (first-n 
		 (multiple-value-bind (first last)
		     (first-n (reverse (sort (hash-table-values *unknown-words*) #'< :key (compose #'length #'third)))
			      (or skip 0))
		   (declare (ignore first))
		   last)
		 n)))

(defun writeup-unknown-word (entry)
  (destructuring-bind (word sentence titles)
      entry
    (let ((formatted-sentence
	   (cl-ppcre:regex-replace-all
	    " ([\\.\\!\\?\\,\\:\\;])"
	    (cl-ppcre:regex-replace-all " ([\\-])? "
					(string-join " " (mapcar #'(lambda (w)
								     (if (equal w word)
									 (concatenate 'string
										      "'''''[["
										      word
										      "]]'''''")
									 w))
								 sentence))
					"\\1")
	    "\\1"))
	  (formatted-links (concatenate
			    'string
			    (string-join ", "
					 (loop
					    :for title :in (reverse titles)
					    :for i :from 1
					    :until (> i 10)
					    :collect (format nil "[[w:~a|~d]]" title i)))
			    (if (> (length titles) 10)
				"…"
				"")))
	  (no-titles (length titles)))
      (format nil "# '''''[[~a]]''''' (~a sider: ~a). \"~a\"
" word no-titles formatted-links formatted-sentence))))


(defun filter-redlink-list (titles &key (bunch-size 50) (extra-delay 5) (key #'identity))
  (when (null titles)
    (return-from filter-redlink-list nil))
  (collecting
    (dolist (bunch (bunch titles bunch-size))
      (dolist (entry (extract '(:query :pages) (api-query :query :prop
							  :prop :info
							  :titles (mapcar key bunch))))
	(when (assoc :missing (cdr entry))
	  (let ((title (cdr (assoc :title (cdr entry)))))
	    (let ((entry (find title titles :test #'equal :key key)))
	      (if (not entry)
		  (format t "WARNING: \"~a\" was not in title list ~a, what is going on?~%" title titles)
		  (collect entry))))))
      (when extra-delay
	(sleep extra-delay)))))

(defun writeup-unknown-words (entries)
  (concatenate 'string
	       (format nil "== Beskrivelse ==
Dette er en automatisk generert liste av ord som ikke ser ut til å ha artikler
på svensk Wiktionary, men som ser ut til å brukes på svensk Wikipedia.
Tanken bak listen er at den kan gjøre det lettere for andre å utvide Wiktionary
ved å rette søkelyset mot ord som foreløpig ikke er beskrevet.

Ordene er sortert etter ''antall artikler'' de forekommer i.
Hvert ord oppgis sammen med dette antallet, én setning som viser ordet i kontekst,
og lenker til opptil ti Wikipedia-artikler der ordet forekommer.
Det er gjort noen forsøk på å sortere ut \"uinteressante\" eller tvilsomme
bruk av ord, som bortfiltrering av referanser (som ofte vil inneholde titler
på andre språk) og av artikler generert av roboter (med \"robotskapad\"-malen).

Datadumper brukes for å generere ordlisten, så det kan forekomme at noe er
utdatert (f.eks. at de lenkede artiklene nå er slettet eller ikke lenger
inneholder ordet).

Denne listen inneholder ~d ord.

Om du implementerer ord fra denne listen og vil endre listen, enten for
å vise at jobben er gjort, eller for å korrigere feil (artikler som faktisk
ikke ''bør'' opprettes), er det praktisk om du bruker overstrykning
eller flytter ordene til en ny seksjon i stedet for å fjerne dem helt.
Boten kan da senere bruke disse sidene for å huske hvilke forslag den allerede
har kommet med, slik at den ikke foreslår de samme ordene om og om igjen.
(Denne funksjonaliteten er imidlertid foreløpig ikke implementert.)

~~~~~~~~

== Ordliste ==
" (length entries))
	       (apply #'concatenate
		      'string
		      (mapcar #'writeup-unknown-word entries))))

(defun writeup-best-unknown-words (n &key (filter t) (limit nil) skip)
  (writeup-unknown-words (first-n (funcall (if filter
					       (papply (filter-redlink-list ? :key #'first))
					       #'identity)
					   (best-unknown-words :n n :skip skip))
				  (or limit
				      n))))

(defun writeup-best-unknown-words-create-page (page-title &key (n 50) skip)
  (assert (begins-with "User:" page-title))
  (api-edit page-title
	    "liste over manglende ord"
	    :createonly t
	    :append (writeup-best-unknown-words (truncate (* 1.1 n))
						:filter t
						:limit n
						:skip skip)))

