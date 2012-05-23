(asdf:defsystem #:wiktionary-bot
  :depends-on (#:svk-util
	       #:cl-utilities
	       #:drakma
	       #:cl-json
	       #:cl-ppcre
	       #:closure-html)
  :serial t
  :components
  ((:file "package")
   (:file "wiktionary")))
	       
  