(in-package :wiktionary-bot)

(defun missing-citations (article)
  (let ((missing 0)
	(total 0))
    (dolist (wikitext (mapcar #'second (remove-if-not #'consp (select-section '(nil "Svenska") (structure-wikitext (swedish-dump-text article))))))
      (let ((templates (top-level-templates wikitext)))
	(labels ((? (template) (find template templates :test #'equal :key #'car)))
	  (unless (or (? "böjning"))
	    (incf total)
	    (when (not (? "citat"))
	      (incf missing))))))
    (values missing total)))

(defun count-missing-citations (&key sample-size)
  (let ((titles (if sample-size
		    (swedish-dump-titles-random-sample sample-size)
		    (swedish-dump-titles :namespace "0")))
	(missing 0)
	(total 0))
    (dolist (title-bunch (bunch titles 1000))
      (log-info 'count-missing-citations
		"processing citation count ~a / ~a" missing total)
      (dolist (title title-bunch)
	(multiple-value-bind (missing-here total-here)
	    (missing-citations title)
	  (incf missing missing-here)
	  (incf total total-here))))
    (values missing total)))
	
      
    