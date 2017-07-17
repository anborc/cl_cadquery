;;;; cadquery.asd

(asdf:defsystem #:cadquery
  :description "Describe cadquery here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:util)
  :serial t
  :components ((:file "package")
		(:file "cadquery")
		(:file "test")
		))

