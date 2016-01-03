(in-package :cl-ode)
(declaim (optimize (speed 3)))

(defclass proto-world ()
  ((contact-group :initform (joint-group-create 0)
		  :initarg  :joint-group
		  :reader   contact-group)
   (step :initform 1/60
	 :initarg  :step
	 :accessor time-step)))
   
				 
(defmethod destroy :before ((this proto-world))
  (destroy (contact-group this))
  (setf (slot-value this 'contact-group) nil))
		    

(create-pointer-type world dWorldID :superclass proto-world)


(defgeneric world-set-defaults (this &key))
(defmethod world-set-defaults ((this world) &key)
  (world-set-gravity this 0 -6 0)
  (world-set-cfm this .001)
  (world-set-damping this .001 .001)
  (world-set-linear-damping-threshold this 0.001)
  (world-set-angular-damping-threshold this .005)
  (world-set-auto-disable-flag this 1))

