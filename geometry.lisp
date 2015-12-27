(in-package :cl-ode)

(defclass proto-geometry () 
  ((surface-mode :initform '(:bounce :rolling :soft-CFM)
		 :initarg :mode
		 :accessor surface-mode
		 :type :int)
   (surface-mu :INITFORM 1d10 :INITARG :mu :ACCESSOR surface-mu)
   (surface-mu2 :INITFORM 0 :INITARG :mu2 :ACCESSOR surface-mu2)
   (surface-rho :INITFORM .1d0 :INITARG :rho :ACCESSOR surface-rho)
   (surface-rho2 :INITFORM 0 :INITARG :rho2 :ACCESSOR surface-rho2)
   (surface-rhon :INITFORM 0 :INITARG :rhon :ACCESSOR surface-rhon)
   (surface-bounce :INITFORM .75 :INITARG :bounce :ACCESSOR surface-bounce)
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
(create-pointer-subclass ray dRayID geometry dGeomID)


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

    (with-accessors ((mode surface-parameters-mode)
		     (bounce surface-parameters-bounce)
		     (bounce-vel surface-parameters-bounce-vel)) params
      
      (setf mode
	    (union (surface-Mode this) (surface-Mode this)))
      
      (setf (surface-parameters-mu params)
	    (* (surface-mu this) (surface-mu that)))
      
	(when (member :mu2 mode)
	  (setf (surface-parameters-mu2 params)
		(* (surface-mu2 this) (surface-mu2 that))))
	
	(when (member :bounce mode)
	  
	  (setf bounce
		(* (surface-bounce this) (surface-bounce that)))
	  
	  (setf bounce-vel
		(max (surface-bounce-vel this) (surface-bounce-vel that))))
	
	(when (member :rolling mode)
	  
	  (setf (surface-parameters-rho params)
		(* (surface-rho this) (surface-rho that)))
	  
	  (setf (surface-parameters-rho2 params)
		(* (surface-rho2 this) (surface-rho2 that)))
	  
	  (setf (surface-parameters-rhoN params)
		(* (surface-rhoN this) (surface-rhoN that)))))
    params)


(defmethod close-callback ((o1 geometry) (o2 geometry))

  ;; get the bodies...if they have one.
  (let* ((b1 (geom-get-body o1))
	 (b2 (geom-get-body o2)))
    
    ;; At least one of these needs a body or movement shouldn't be happening...
    (unless (and (null b1) (null b2))

      ;; create space to hold the collisions...
      (with-foreign-object (contact '(:struct Contact-struct) *default-max-contacts*)
	
	(let* ((surf (make-instance 'surface-parameters :pointer (cffi:foreign-slot-pointer contact '(:struct contact-struct) 'surface)))
	       (geom (print (cffi:foreign-slot-pointer (print contact) '(:struct ode::contact-struct) 'geom)))
	       (geom-obj (make-instance 'contact-geometry :pointer geom)))
	  (combine-physics-objects o1 o2 surf)


	(let ((num-contacts (collide o1
				     o2
				     *default-max-contacts*
				     geom
				     (cffi:foreign-type-size '(:struct contact-geometry-struct)))))

	  (unless (zerop num-contacts)
	    
	  (format t "geo pos: ~A!~%" (cffi:foreign-slot-value  geom '(:struct contact-geometry-struct) 'pos))
	  (format t "geo depth: ~A!~%" (cffi:foreign-slot-value geom '(:struct contact-geometry-struct) 'depth))
	  (format t "g1 depth: ~A!~%" (cffi:foreign-slot-value  geom '(:struct contact-geometry-struct) 'g1))
	  (format t "g2 depth: ~A!~%" (cffi:foreign-slot-value  geom '(:struct contact-geometry-struct) 'g2))

	    (let* ((world (cond (b1 (body-get-world b1))
				(b2 (body-get-world b2))
				(t (error "Can't find a world for these two geometries/bodies!"))))
		   (contact-group (ode::contact-group world)))
	      
	      (dotimes (i num-contacts)
		(print (joint-attach (print (joint-create-contact world contact-group contact)) b1 b2)))))))))))

