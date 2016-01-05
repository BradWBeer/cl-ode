(in-package :cl-ode)
(declaim (optimize (speed 3)))

(defclass proto-geometry () 
  ((ghost :initform nil
	  :initarg :ghost
	  :accessor ghost)
   (collision-handler :initform nil
		      :initarg :collision-handler
		      :accessor collision-handler)
   (surface-mode :initform '(:bounce :rolling)
		 :initarg :mode
		 :accessor surface-mode
		 :type :int)
   (surface-mu :INITFORM (infinity) :INITARG :mu :ACCESSOR surface-mu)
   (surface-mu2 :INITFORM 0 :INITARG :mu2 :ACCESSOR surface-mu2)
   (surface-rho :INITFORM .1 :INITARG :rho :ACCESSOR surface-rho)
   (surface-rho2 :INITFORM 0 :INITARG :rho2 :ACCESSOR surface-rho2)
   (surface-rhon :INITFORM 0 :INITARG :rhon :ACCESSOR surface-rhon)
   (surface-bounce :INITFORM .9 :INITARG :bounce :ACCESSOR surface-bounce)
   (surface-bounce-vel :INITFORM .01 :INITARG :bounce-vel :ACCESSOR
		       surface-bounce-vel)
   (surface-soft-erp :INITFORM 0 :INITARG :soft-erp :ACCESSOR
		     surface-soft-erp)
   (surface-soft-cfm :INITFORM .001 :INITARG :soft-cfm :ACCESSOR
		     surface-soft-cfm)
   (surface-motion1 :INITFORM 0 :INITARG :motion1 :ACCESSOR
		    surface-motion1)
   (surface-motion2 :INITFORM 0 :INITARG :motion2 :ACCESSOR
		    surface-motion2)
   (surface-motionn :INITFORM 0 :INITARG :motionn :ACCESSOR
		    surface-motionn)
   (surface-slip1 :INITFORM 0 :INITARG :slip1 :ACCESSOR surface-slip1)
   (surface-slip2 :INITFORM 0 :INITARG :slip2 :ACCESSOR surface-slip2)))

(create-pointer-type geometry dGeomID :destructor Geom-Destroy :superclass proto-geometry)

(create-pointer-subclass sphere dSphereID geometry dGeomID)
(create-pointer-subclass box dBoxID geometry dGeomID)
(create-pointer-subclass plane dPlaneID geometry dGeomID)
(create-pointer-subclass cylinder dCylinderID geometry dGeomID)
(create-pointer-subclass capsule dCapsuleID geometry dGeomID)
(create-pointer-subclass convex dConvexID geometry dGeomID)

(defmethod body-get-transform ((this geometry))
  (let ((position (body-get-position this))
	(rotation (body-get-rotation this)))
    (make-array 16
		:element-type 'single-float
		:initial-contents (list (elt rotation 0) (elt rotation 4) (elt rotation 8)  0.0
					(elt rotation 1) (elt rotation 5) (elt rotation 9)  0.0
					(elt rotation 2) (elt rotation 6) (elt rotation 10) 0.0
					(elt position 0) (elt position 1) (elt position 2)  1.0))))

(defmethod body-set-transform ((this geometry) (m array))

  (body-set-position this (elt m 12) (elt m 13) (elt m 14))
  (body-set-rotation this m))


(defmethod combine-physics-objects ((this geometry) (that geometry) params)

    (cffi:with-foreign-slots ((mode mu mu2 rho rho2 rhoN bounce bounce-vel soft-erp soft-cfm motion1 motion2 motionN slip1 slip2)
			      params
			      (:struct surface-parameters-struct))
      

      (setf mode
	    (union (surface-Mode this) (surface-Mode this)))

      (setf soft-cfm (max (surface-soft-cfm this) (surface-soft-cfm that)))
      
      (setf mu       
	    (max (surface-mu this) (surface-mu that)))
      
	(when (member :mu2 mode)
	  (setf mu2
		(max (surface-mu2 this) (surface-mu2 that))))
	
	(when (member :bounce mode)
	  
	  (setf bounce
		(* (surface-bounce this) (surface-bounce that)))
	  
	  (setf bounce-vel
		(max (surface-bounce-vel this) (surface-bounce-vel that))))
	
	(when (member :rolling mode)
	  
	  (setf rho
		(* (surface-rho this) (surface-rho that)))
	  
	  (setf rho2
		(* (surface-rho2 this) (surface-rho2 that)))
	  
	  (setf rhoN
		(* (surface-rhoN this) (surface-rhoN that)))))
    params)

(defmethod close-callback ((o1 geometry) (o2 geometry))

  ;; get the bodies...if they have one.
  (let* ((b1 (geom-get-body o1))
	 (b2 (geom-get-body o2)))
    
    ;; At least one of these needs a body or movement shouldn't be happening...
    (when (and (or b1 b2)
	       (or (and b1 (body-is-enabled b1))
		   (and b2 (body-is-enabled b2))))
		   

      ;; create space to hold the collisions...
      (with-foreign-object (contact '(:struct Contact-struct) *default-max-contacts*)
	
	(let* ((surf (cffi:foreign-slot-pointer contact '(:struct contact-struct) 'surface))
	       (geom (cffi:foreign-slot-pointer contact '(:struct ode::contact-struct) 'geom)))

	  (combine-physics-objects o1 o2 surf)

	  (let ((num-contacts (collide o1
				     o2
				     *default-max-contacts*
				     geom
				     (cffi:foreign-type-size '(:struct contact-struct)))))

	  (unless (zerop num-contacts)	    
	    
	    (let* ((world (cond (b1 (body-get-world b1))
				(b2 (body-get-world b2))
				(t (error "Can't find a world for these two geometries/bodies!"))))
		   (contact-group (ode::contact-group world)))
	      
	      (dotimes (i num-contacts)
		
		(let* ((current-contact (cffi:mem-aptr contact '(:struct Contact-Struct) i))
		       (cc-object (make-instance 'contact :pointer current-contact)))

		  (when (collision-handler o1) (funcall (collision-handler o1) o1 o2 current-contact)) ;;cc-object))
		  (when (collision-handler o2) (funcall (collision-handler o2) o2 o1 current-contact)) ;;cc-object))
		
		  (unless (or (ghost o1) (ghost o2))
		    (joint-attach (joint-create-contact world contact-group current-contact) b1 b2))))))))))))

