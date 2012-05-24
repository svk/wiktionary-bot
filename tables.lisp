(in-package :wiktionary-bot)
		      
(def-table-recognizer parse-swedish-noun-table
    (compose #'trim #'lhtml->text)
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

