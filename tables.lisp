(in-package :wiktionary-bot)

(defun links-from-table-cell (cell)
  (mapcar (compose #'trim #'third)
	  (lhtml-select (list* :virtual nil cell)
			:name :a
			:list t)))
		      
(def-table-recognizer parse-swedish-noun-table
    #'links-from-table-cell
  (("Böjningar" "Singular" "Singular" "Plural" "Plural")
   ("utrum" "Obestämd" "Bestämd" "Obestämd" "Bestämd")
   ("Nominativ" (:singular :indefinite :nominative)
		(:singular :definite :nominative)
		(:plural :indefinite :nominative)
		(:plural :definite :nominative))
   ("Genitiv" (:singular :indefinite :genitive)
	      (:singular :definite :genitive)
	      (:plural :indefinite :genitive)
	      (:plural :definite :genitive))))

(def-table-recognizer parse-swedish-verb-table
    #'links-from-table-cell
  (("Böjningar" "Aktiv" "Passiv")
   ("Infinitiv" (:active :infinitive) (:passive :infinitive))
   ("Presens" (:active :present) (:passive :present))
   ("Preteritum" (:active :past) (:passive :past))
   ("Supinum" (:active :supinum) (:passive :supinum))
   ("Imperativ" (:active :imperative) nil)
   ("Particip" "Particip" "Particip")
   ("Presens" (:present :participle) nil)
   ("Perfekt" (:perfect :participle) nil)))

(def-table-recognizer parse-swedish-adjective-noncomparative-table
    #'links-from-table-cell
  (("Böjningar" "Böjningar" "Positiv")
   ("Böjningar" "Böjningar" "Attributivt")
   ("Obestämd" "Utrum" (:positive :utrum :indefinite :singular))
   ("Obestämd" "Neutrum" (:positive :neutrum :indefinite :singular))
   ("Bestämd" "Mask" (:positive :masculine :definite :singular))
   ("Bestämd" "Alla" (:positive :all :definite :singular))
   ("Plural" "Plural" (:positive :plural))
   (nil nil "Predikativt")
   ("Singular" "Utrum" (:positive :utrum :predicative :singular))
   ("Singular" "Neutrum" (:positive :neutrum :predicative :singular))
   ("Plural" "Plural" (:positive :plural))
   ("Kompareras" nil nil)))

(def-table-recognizer parse-swedish-adjective-table
    #'links-from-table-cell
  (("Böjningar" "Böjningar" "Positiv" "Komparativ" "Superlativ")
   ("Böjningar" "Böjningar" "Attributivt" "Attributivt" "Attributivt")
   ("Obestämd" "Utrum"
	       (:positive :utrum :indefinite :singular)
	       (:comparative :utrum :indefinite :singular)
	       (:superlative :utrum :indefinite :singular))
   ("singular" "Neutrum"
	       (:positive :neutrum :indefinite :singular)
	       (:comparative :neutrum :indefinite :singular)
	       (:superlative :neutrum :indefinite :singular))
   ("Bestämd" "Maskulinum"
	      (:positive :masculine :indefinite :singular)
	      (:comparative :masculine :indefinite :singular)
	      (:superlative :masculine :indefinite :singular))
   ("singular" "Alla"
	       (:positive :all :indefinite :singular)
	       (:comparative :all :indefinite :singular)
	       (:superlative :all :indefinite :singular))
   ("Plural" "Plural"
	     (:positive :plural)
	     (:comparative :plural)
	     (:superlative :plural))
   (nil nil "Predikativt" "Predikativt" "Predikativt")
   ("Singular" "Utrum"
	       (:positive :utrum :predicative :singular)
	       (:comparative :utrum :predicative :singular)
	       (:superlative :utrum :predicative :singular))
   ("Singular" "Neutrum"
	       (:positive :neutrum :predicative :singular)
	       (:comparative :neutrum :predicative :singular)
	       (:superlative :neutrum :predicative :singular))
   ("Plural" "Plural" 
	       (:positive :neutrum :predicative :plural)
	       (:comparative :neutrum :predicative :plural)
	       (:superlative :neutrum :predicative :plural))
   (nil nil nil nil nil)
   ("Adverbavledning" "Adverbavledning"
		      (:adverbial)
		      nil nil)))
   


	       