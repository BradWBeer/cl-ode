(in-package #:cl-ode)

(defvar is-double-precision? nil)
(defvar *object-hash*)

(defun number->dreal (x)
  (coerce x 'double-float))

(defun number->single-float (x)
  (coerce x 'single-float))


(defctype dreal (:wrapper :float
			  :to-c  number->single-float))
;; (defctype dreal (:wrapper :double
;; 			  :to-c number->dreal
;; 			  :from-c number->single-float))


(defmacro infinity (&optional (precision is-double-precision?))
  `(if (eql ,precision :single)
       (progn
         #+sbcl sb-ext:single-float-positive-infinity
         #+clozure 1S++0
         #+abcl ext:single-float-positive-infinity
         #+allegro excl::*infinity-single*
         #+cmu ext:single-float-positive-infinity
         #+(and ecl (not infinity-not-available)) si:single-float-positive-infinity
         #+lispworks (coerce infinity$$ 'single-float)
         #+scl ext:single-float-positive-infinity
         #+t most-positive-single-float)
       (progn
         #+sbcl sb-ext:double-float-positive-infinity
         #+clozure 1D++0
         #+abcl ext:double-float-positive-infinity
         #+allegro excl::*infinity-double*
         #+cmu ext:double-float-positive-infinity
         #+(and ecl (not infinity-not-available)) si:double-float-positive-infinity
         #+lispworks #.(read-from-string "10E999")
         #+scl ext:double-float-positive-infinity
         #+t most-positive-double-float)))


(defbitfield Contact-Enum
  (:Mu2		   #x001)
  (:ContactAxisDep #x001)
  (:FDir1	   #x002)
  (:Bounce	   #x004)
  (:Soft-ERP	   #x008)
  (:Soft-CFM	   #x010)
  (:Motion1	   #x020)
  (:Motion2	   #x040)
  (:MotionN	   #x080)
  (:Slip1	   #x100)
  (:Slip2	   #x200)
  (:Rolling        #x400)
  ;;(:Approx0	   #x0000)
  (:Approx1-1	   #x1000)
  (:Approx1-2	   #x2000)
  (:Approx1-N      #x4000)
  (:Approx1        #x7000))

  
(defctype dVector3 (:array dReal 4))
(defctype dVector4 (:array dReal 4))
(defctype dMatrix3 (:array dReal 12))
(defctype dMatrix4 (:array dReal 16))
(defctype dMatrix6 (:array dReal 48))
(defctype dQuaternion (:array dReal 4))

(defgeneric destroy (this))


(define-condition ode-error (error)
  ((error-string :initarg :error-string :reader error-string))
  (:report (lambda (c stream)
	     (format stream "ode function returned error ~A"
		     (error-string c)))))

(defmacro create-pointer-type (name id &key destructor superclass)
  (let ((type-name (intern (string-upcase (concatenate 'string (princ-to-string name) "-TYPE")))))
  `(progn 

     (define-foreign-type ,type-name () ()
			  (:actual-type :pointer)
			  (:simple-parser ,id))
     
     (defclass ,name ,(when superclass (list superclass))
       ((pointer :initform (error 'ode-error ,(concatenate 'string (princ-to-string name)
							   " object created without pointer!"))
		 :initarg :pointer)))
     
     (defmethod initialize-instance :after ((this ,name) &key)
		(setf (gethash (cffi:pointer-address (slot-value this 'pointer)) *object-hash*) this))

     (defmethod translate-to-foreign ((val null) (type ,type-name))
       (null-pointer))

     (defmethod translate-to-foreign ((this ,name) (type ,type-name))
       (slot-value this 'pointer))
     
     (defmethod translate-from-foreign (pointer (type ,type-name))
         (unless (null-pointer-p pointer)
	   (or (gethash (cffi:pointer-address pointer) *object-hash*)
	       (setf (gethash (cffi:pointer-address pointer) *object-hash*) (make-instance ',name :pointer pointer)))))

     (defmethod destroy ((this ,name))
       (remhash (cffi:pointer-address (slot-value this 'pointer)) *object-hash*)
       (,(or
	  destructor
	  (intern (string-upcase (concatenate 'string (princ-to-string name) "-destroy")))) this)
       (setf (slot-value this 'pointer) nil)))))

(defmacro create-pointer-subclass (name id parent parent-id)
  (let ((type-name (intern (string-upcase (concatenate 'string (princ-to-string name) "-TYPE")))))
    `(progn
       (define-foreign-type ,type-name () ()
			    (:actual-type ,parent-id)
			    (:simple-parser ,id))

       (defclass ,name (,parent) ())
       (defmethod translate-from-foreign (pointer (type ,type-name))
	 (if (null-pointer-p pointer)
	     (error 'ode-error :error-string "ODE Function returned a NULL!")
	     (make-instance ',name :pointer pointer)))

       (defmethod translate-to-foreign ((this ,name) (type ,type-name))
	 (slot-value this 'pointer)))))



(create-pointer-type geometry dGeomID :destructor Geom-Destroy :superclass proto-geometry)


(create-pointer-subclass sphere dSphereID geometry dGeomID)
(create-pointer-subclass box dBoxID geometry dGeomID)
(create-pointer-subclass plane dPlaneID geometry dGeomID)
(create-pointer-subclass cylinder dCylinderID geometry dGeomID)
(create-pointer-subclass capsule dCapsuleID geometry dGeomID)
(create-pointer-subclass ray dRayID geometry dGeomID)


(defmacro struct-slot (name type slot)
  `(defmacro ,name (this) 
       `(foreign-slot-value (slot-value ,this 'pointer)
		      ',,type ',,slot)))

(defmacro create-struct-class ((name id) &body members)
  (let ((type-name (intern (string-upcase (concatenate 'string (princ-to-string name) "-TYPE"))))
	(struct-name (intern (string-upcase (concatenate 'string (princ-to-string name) "-STRUCT")))))
    `(progn
       
       (defcstruct ,struct-name
	,@members)
       
       (define-foreign-type ,type-name () ()
		     (:actual-type :pointer)
		     (:simple-parser ,id))

       (defclass ,name ()
	 ((pointer :initform (foreign-alloc '(:struct ,struct-name))
		   :initarg :pointer)
	  (len :initform 1
	       :initarg :len
	       :reader len)))
       
       (defmethod initialize-instance ((this ,name) &key (len 1))
	 (setf (slot-value this 'pointer) (foreign-alloc '(:struct ,struct-name) :count (setf (slot-value this 'len) len))))
       
       (defmethod translate-to-foreign ((this ,name) (type ,type-name))
	 (slot-value this 'pointer))
       
       (defmethod translate-from-foreign (pointer (type ,type-name))
	 (unless (null-pointer-p pointer)
	   (make-instance ',name :pointer pointer)))

       (defmethod struct-aref ((this ,name) (num integer))
	 (cond ((zerop num) this)
	       ((< num 0) (error "Negitive index detected!"))
	       ((>= num (len this)) (error "Index beyond bounds!"))
	       (t (make-instance ',name :pointer (cffi:mem-aptr (slot-value this 'pointer) '(:struct ,struct-name) num)))))

       (defmethod destroy ((this ,name))
	 (foreign-free (slot-value this 'pointer))
	 (setf (slot-value this 'pointer) nil))

       ,@(loop for (var type) in members
	    append (let ((accessor (intern
				     (string-upcase
				      (concatenate 'string
						   (princ-to-string name)
						   "-"
						   (princ-to-string var))))))
		      `((defmethod ,accessor ((this ,name))
			 (cffi:with-foreign-slots ((,var) (slot-value this 'ode::pointer) (:struct ,struct-name))
			   ,var))
			
			(defmethod (setf ,accessor) (val (this ,name))
			  (cffi:with-foreign-slots ((,var) (slot-value this 'ode::pointer) (:struct ,struct-name))
			    
			    (setf ,var val)))))))))

(create-struct-class (mass dMass)
  (mass dReal)
  (center dVector3)
  (inertia dMatrix3))


(create-struct-class (surface-parameters dSurfaceParameters)
  (mode Contact-Enum)
  (mu dReal)
  (mu2 dReal)
  (rho dReal)
  (rho2 dReal)
  (rhoN dReal)
  (bounce dReal)
  (bounce-vel dReal)
  (soft-erp dReal)
  (soft-cfm dReal)
  (motion1 dReal)
  (motion2 dReal)
  (motionN dReal)
  (slip1 dReal)
  (slip2 dReal))

(create-struct-class (contact-geometry dContactGeom)
  (pos dVector3)
  (normal dVector3)
  (depth dReal)
  (g1 dGeomID)
  (g2 dGeomID)
  (side1 :int)
  (side2 :int))


(create-struct-class (contact dContact)
  (surface (:struct surface-parameters-struct))
  (geom (:struct contact-geometry-struct))
  (fdir1 dVector3))
