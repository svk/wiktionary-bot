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
	       #:cl-irc
	       #:closure-html)
  :serial t
  :components
  ((:file "package")
   (:file "locks")
   (:file "queue")
   (:file "log")
   (:file "cqueue")
   (:file "lhtml")
   (:file "wiktionary")
   (:file "markup")
   (:file "tokens")
   (:file "dumps")
   (:file "tables")
   (:file "reverse-inflect")
   (:file "conjugation")
   (:file "media")
   (:file "irc")
   (:file "unknown")
   (:file "auto")
   (:file "tasks")
   (:file "recent")))
	       
  