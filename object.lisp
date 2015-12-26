(in-package #:cl-ode)

(defparameter mode-options 
  '(ode::Mu2	  
    ode::Axis-Dep 
    ode::FDir1	  
    ode::Bounce  
    ode::Soft-ERP 
    ode::Soft-CFM 
    ode::Motion1 
    ode::Motion2 
    ode::MotionN 
    ode::Slip1	  
    ode::Slip2	  
    ode::Rolling 
    ode::Approx0   
    ode::Approx1-1 
    ode::Approx1-2 
    ode::Approx1-N 
    ode::Approx1))


(defparameter surface-parameters-macros
  '(surface-parameters-Mu2	  
    surface-parameters-Axis-Dep 
    surface-parameters-FDir1	  
    surface-parameters-Bounce  
    surface-parameters-Soft-ERP 
    surface-parameters-Soft-CFM 
    surface-parameters-Motion1 
    surface-parameters-Motion2 
    surface-parameters-MotionN 
    surface-parameters-Slip1	  
    surface-parameters-Slip2	  
    surface-parameters-Rolling 
    surface-parameters-Approx0   
    surface-parameters-Approx1-1 
    surface-parameters-Approx1-2 
    surface-parameters-Approx1-N 
    surface-parameters-Approx1))

;; (defmethod initialize-instance :around ((this physics-object) &key position rotation matrix offset-matrix offset-position offset-rotation)

;;   (call-next-method)

;;   (when (body this)
;;     (ref (body this)))

;;   (unless (typep this 'physics-plane)
;;     (if matrix
;; 	(if (body this)
;; 	    (set-transform (body this) matrix)
;; 	    (set-transform this matrix))
;; 	(progn
;; 	  (when position
;; 	    (if (body this)
;; 		(body-set-position (pointer (body this))
;; 				   (first (or position 0))
;; 				   (second (or position 0))
;; 				   (third (or position 0)))
;; 		(geom-set-position (geometry this)
;; 				   (first (or position 0))
;; 				   (second (or position 0))
;; 				   (third (or position 0)))))
;; 	  (when rotation (error "setting rotation is not yet implemented!"))))

;;     (when (body this)
;;       (cond (offset-matrix (set-offset-transform this offset-matrix))
;; 	    (offset-position (geom-set-offset-position (geometry this)
;; 						       (first offset-position)
;; 						       (second offset-position)
;; 						       (third offset-position)))
;; 	    (offset-rotation (error "setting rotation is not yet implemented!"))
;; 	    (t t))))





;;   (setf (gethash (pointer-address (geometry this)) *physics-geometry-hash*) this))


;; (defmethod unload ((this physics-object) &key)
;;   (when (body this)
;;     (unref (body this))

;;     (setf (slot-value this 'body) nil))

;;   (when (geometry this)
;;     (when (gethash (pointer-address (geometry this)) *physics-geometry-hash*)
;;       (remhash (pointer-address (geometry this)) *physics-geometry-hash*))

;;     (Geom-Destroy (geometry this))
;;     (setf (slot-value this 'geometry) nil)))

(defmethod get-transform ((geom physics-object))
  (let ((position (geom-get-position (geometry geom)))
	(rotation (geom-get-rotation (geometry geom))))
    (sb-cga:matrix (n->sf (elt rotation 0)) (n->sf (elt rotation 1)) (n->sf (elt rotation 2))  (n->sf (elt position 0)) 
		   (n->sf (elt rotation 4)) (n->sf (elt rotation 5)) (n->sf (elt rotation 6))  (n->sf (elt position 1))
		   (n->sf (elt rotation 8)) (n->sf (elt rotation 9)) (n->sf (elt rotation 10)) (n->sf (elt position 2))
		   (n->sf 0)                (n->sf 0)                (n->sf 0)                 (n->sf 1))))


(defmethod set-transform ((geom physics-object) (matrix SIMPLE-ARRAY))
  (with-foreign-object (rot 'dReal 12)
    (loop for i from 0 to 11
       do (setf (mem-aref rot 'dreal i) 0))

    (loop
       for pos in '(0 4 8 1 5 9 2 6 10)
       for i from 0 to 11
	 
       do (setf (mem-aref rot 'dreal i) (elt matrix pos)))

    (geom-set-rotation (geometry geom) rot))
  
  (with-foreign-object (pos 'dreal 4)
    (loop for i from 0 to 3 do (setf (mem-aref pos 'dreal i) 0))

    (geom-set-position (geometry geom)
		       (elt matrix 3)
		       (elt matrix 7)
		       (elt matrix 11))))



(defmethod set-offset-transform ((geom physics-object) (matrix SIMPLE-ARRAY))
  (with-foreign-object (rot 'dReal 12)
    (loop for i from 0 to 11
       do (setf (mem-aref rot 'dreal i) 0))

    (loop
       for pos in '(0 4 8 1 5 9 2 6 10)
       for i from 0 to 11
	 
       do (setf (mem-aref rot 'dreal i) (elt matrix pos)))

    (geom-set-rotation (geometry geom) rot))
  
  (with-foreign-object (pos 'dreal 4)
    (loop for i from 0 to 3 do (setf (mem-aref pos 'dreal i) 0))

    (geom-set-position (geometry geom)
		       (elt matrix 3)
		       (elt matrix 7)
		       (elt matrix 11))))


(defmethod set-position ((this physics-object) x y z)
  (with-slots ((body body)) this
    (if body 
	(body-set-position (pointer body) x y z)
	(geom-set-position (geometry this) x y z))))

(defmethod combine-physics-objects ((this geometry) (that geometry))

  (let ((params (make-instance 'surface-parameters)))
    
    (setf (surface-parameters-mode this)
	  (cffi:foreign-bitfield-value 'ode::Contact-Enum
				       (union (surface-mode this) (surface-mode that))))

    
    ))

  (setf (cffi:foreign-slot-value surface '(:struct ode:dSurfaceParameters) 'ode::mode)
	(cffi:foreign-bitfield-value 'ode::Contact-Enum
				     (union (surface-mode this) (surface-mode that))))
  
  (combine-surface-properties surface
			      (surface-mu this)
			      (surface-mu that)
			      'ode::mu)
  
  (when (or (member :bounce (surface-mode this))
	    (member :bounce (surface-mode that)))

    (combine-surface-properties surface
				(surface-bounce this)
				(surface-bounce that)
				'ode::bounce)

    (combine-surface-properties surface
				(surface-bounce-vel this)
				(surface-bounce-vel that)
				'ode::bounce-vel))

  (when (or (member :soft-cfm (surface-mode this))
  	    (member :soft-cfm (surface-mode that)))
    (combine-surface-properties surface 
  				(surface-soft-cfm this)
  				(surface-soft-cfm that)
  				'ode::soft-cfm))


  (when (or (member :mu2 (surface-mode this))
  	    (member :mu2 (surface-mode that)))
    (combine-surface-properties surface
  				(surface-mu2 this)
  				(surface-mu2 that)
  				'ode::mu2))
  
  (when (or (member :rolling (surface-mode this))
  	    (member :rolling (surface-mode that)))

    (combine-surface-properties surface
    				(surface-rho this)
    				(surface-rho that)
    				'ode::rho)
    
    (combine-surface-properties surface
    				(surface-rho2 this)
    				(surface-rho2 that)
    				'ode::rho2)
    
    (combine-surface-properties surface
    				(surface-rhoN this)
    				(surface-rhoN that)
    				'ode::rhoN)))


(defmethod close-callback ((this physics-object) (that physics-object))

  (let* ((o1 (geometry this))
	 (o2 (geometry that))
	 (b1 (geom-get-body o1))
	 (b2 (geom-get-body o2)))
    
    (with-foreign-object (contact '(:struct dContact) *physics-default-max-contacts*)
      
      (let* ((surface (foreign-slot-pointer contact '(:struct ode::dContact) 'ode::surface))
	     (gg (foreign-slot-pointer contact '(:struct ode::dContact) 'ode::geom)))
	
	(combine-physics-objects surface this that)
	
	(let ((num-contacts (collide o1 o2 *physics-default-max-contacts* gg (foreign-type-size '(:struct ode::dContact)))))

	  (unless (zerop num-contacts)

	    (when (or (and (not (cffi:null-pointer-p b1)) (not (zerop (body-is-kinematic b1))))
		      (and (not (cffi:null-pointer-p b2)) (not (zerop (body-is-kinematic b2)))))
	      
	      (when (cffi:null-pointer-p b1)
		(let* ((oldvel (body-get-linear-vel b2))
		       (norm (foreign-slot-value gg '(:struct ode::dContactGeom) 'ode::normal))
		       (depth (foreign-slot-value gg '(:struct ode::dContactGeom) 'ode::depth))
		       (pos (body-get-position b2))
		       (vel (remove-vector oldvel norm))
		       (dpos (sb-cga:vec* (make-vector (aref norm 0) (aref norm 1) (aref norm 2))
					  (coerce (- depth) 'single-float))))
		  
		  
		  ;;(format t "geom=~A body=~A geometry=~A~%" o2 pos (geom-get-position o2))
		  (when (> (abs (aref pos 2)) 1)
		    
		    
					;(format t "~A ov=~A n=~A nv=~A~%" pos oldvel norm vel)
					;(print (aref vel 2))
		    )
		  
		  ;;(format t "#1 ov=~A v=~A p=~A depth=~A~%" oldvel vel (body-get-position b2) (foreign-slot-value gg '(:struct ode::dContactGeom) 'ode::depth))
		  
		  (body-set-linear-vel b2 (aref vel 0) (aref vel 1) (aref vel 2))))
	      ;; (body-set-position b2 0
	      ;; 			   (+ (aref pos 0)
	      ;; 			      (aref dpos 0))
	      ;; 			   (+ (aref pos 1)
	      ;; 			      (aref dpos 1))
	      ;; 			   (+ (aref pos 2)
	      ;; 			      (aref dpos 2)))))
	      
	      
	      ;;(print (foreign-slot-value gg '(:struct ode::dContactGeom) 'ode::normal))

	      
	      
	      
	      (when (cffi:null-pointer-p b2)
		(let* ((oldvel (body-get-linear-vel b1))
		       (norm (foreign-slot-value gg '(:struct ode::dContactGeom) 'ode::normal))
		       (vel (remove-vector oldvel (make-vector (- (aref norm 0))
								      (- (aref norm 1))
								      (- (aref norm 2)))))

		       
		       (pos (body-get-position b1)))
		  
		  ;;(format t "geom=~A body=~A geometry=~A~%" o1 pos (geom-get-position o1))
		  (when (> (abs (aref pos 2)) 1)
					;(format t "~A ov=~A n=~A nv=~A~%" pos oldvel norm vel)
					;(print (aref vel 2))
		    )
		  ;; (print (foreign-slot-value gg '(:struct ode::dContactGeom) 'ode::normal))
		  ;;(format t "#2 ov=~A v=~A p=~A depth=~A~%" oldvel vel (body-get-position b1) (foreign-slot-value gg '(:struct ode::dContactGeom) 'ode::depth))
		  (body-set-linear-vel b1 (aref vel 0) (aref vel 1) (aref vel 2)))))
	    

	    (loop for x from 0 to (1- num-contacts)
	       do (joint-attach
		   (joint-create-contact *physics-world* *physics-contact-group* (cffi:mem-aptr contact '(:struct dContact) x))
		   b1 b2))))))))


(defmethod enabled ((this physics-object))
  (let ((pbody (pointer (body this))))

    (eq 1 (Body-Is-Enabled pbody))))
