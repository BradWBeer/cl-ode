;;;; cl-ode.asd

(asdf:defsystem #:cl-ode
  :description "Describe cl-ode here"
  :author "Brad Beer (WarWeasle)"
  :license "Specify license here"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "package")
	       (:file "library")
	       (:file "types")
	       (:file "mass")
	       (:file "world")
	       (:file "spaces")
	       (:file "body")
	       (:file "geometry")
	       (:file "joints")
	       (:file "bindings")
	       (:file "utils")

	       ;;(:file "object")
	       ;; (:file "sphere")
	       ;; (:file "cylinder")
	       ;; (:file "capsule")
	       ;; (:file "box")
	       ;; (:file "plane")
	       ;; (:file "ray")
	       ;; (:file "spring")
	       ;; (:file "shapes")
	       (:file "ode")
	       ))


