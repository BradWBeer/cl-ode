;;;; cl-ode.asd

(asdf:defsystem #:cl-ode
  :description "Describe cl-ode here"
  :author "Brad Beer (WarWeasle)"
  :license "Specify license here"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "package")
	       (:file "library")
	       (:file "utils")
	       (:file "types")
	       (:file "world")
	       (:file "spaces")
	       (:file "body")
	       (:file "geometry")
	       (:file "joints") 
	       (:file "bindings")
	       (:file "ray")
	       (:file "spring")
	       (:file "ode")))


