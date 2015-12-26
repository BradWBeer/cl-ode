(in-package :cl-ode)

(defclass proto-geometry () 
  ((surface-mode :initform '(:bounce :soft-CFM)
		 :initarg :mode
		 :accessor surface-mode
		 :type :int)
   (surface-mu :INITFORM 1d100 :INITARG :mu :ACCESSOR surface-mu)
   (surface-mu2 :INITFORM 0 :INITARG :mu2 :ACCESSOR surface-mu2)
   (surface-rho :INITFORM .1d0 :INITARG :rho :ACCESSOR surface-rho)
   (surface-rho2 :INITFORM 0 :INITARG :rho2 :ACCESSOR surface-rho2)
   (surface-rhon :INITFORM 0 :INITARG :rhon :ACCESSOR surface-rhon)
   (surface-bounce :INITFORM 0 :INITARG :bounce :ACCESSOR surface-bounce)
   (surface-bounce-vel :INITFORM 0 :INITARG :bounce-vel :ACCESSOR
		       surface-bounce-vel)
   (surface-soft-erp :INITFORM 0 :INITARG :soft-erp :ACCESSOR
		     surface-soft-erp)
   (surface-soft-cfm :INITFORM 0 :INITARG :soft-cfm :ACCESSOR
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

(defmethod combine-physics-objects ((this geometry) (that geometry))

  (let ((params (make-instance 'surface-parameters)))
    
    
;;    	  (cffi:foreign-bitfield-value 'Contact-Enum
    ;;(union (surface-mode this) (surface-mode that))))
    params
    
    ))



;; (defmacro combine-surface-properties (surface val1 val2 property)
;;   (let ((v1 (gensym))
;; 	(v2 (gensym)))
    
;;     `(setf (foreign-slot-value ,surface '(:struct ode::dSurfaceParameters) ,property)
;; 	   (let ((,v1 ,val1)
;; 		 (,v2 ,val2))
;; 	     (cond ((and ,v1 ,v2) (/ (+ ,v1 ,v2) 2))
;; 		   ((and (null ,v1) ,v2) ,v2)
;; 		   ((and (null ,v2) ,v1) ,v1)
;; 		   (t 0))))))



