(asdf:defsystem #:wiktionary-bot
  :depends-on (#:svk-util
	       #:cl-utilities
	       #:cl-unicode
	       #:lispbuilder-yacc
	       #:drakma
	       #:cl-json
	       #:cl-ppcre
	       #:rfc3339-timestamp
	       #:cxml
	       #:closure-html)
  :serial t
  :components
  ((:file "package")
   (:file "wiktionary")
   (:file "markup")
   (:file "tokens")
   (:file "dumps")
   (:file "tables")
   (:file "conjugation")
   (:file "unknown")
   (:file "auto")))
	       
  