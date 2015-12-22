;;;; cl-ode.asd

(asdf:defsystem #:cl-ode
  :description "Describe cl-ode here"
  :author "Brad Beer (WarWeasle)"
  :license "Specify license here"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "package")
	       (:file "library")
	       (:file "bindings")
	       (:file "utils")
	       (:file "world")
	       ;; (:file "ode")
	       ;; (:file "mass")
	       ;; (:file "body")
	       ;; (:file "object")
	       ;; (:file "sphere")
	       ;; (:file "cylinder")
	       ;; (:file "capsule")
	       ;; (:file "box")
	       ;; (:file "plane")
	       ;; (:file "ray")
	       ;; (:file "spring")
	       ;; (:file "shapes")

	       ))


