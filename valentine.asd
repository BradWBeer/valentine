;;;; valentine.asd

(asdf:defsystem #:valentine
  :description "Describe valentine here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "package")
	       (:file "library")
	       (:file "swigbullet")
	       ;;(:file "swigbullet-clos")
	       (:file "valentine")
	       ))

