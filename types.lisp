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
  (:Mu2		 #x001)
  (:FDir1	 #x002)
  (:Bounce	 #x004)
  (:Soft-ERP	 #x008)
  (:Soft-CFM	 #x010)
  (:Motion1	 #x020)
  (:Motion2	 #x040)
  (:MotionN	 #x080)
  (:Slip1	 #x100)
  (:Slip2	 #x200)
  (:Rolling      #x400)
  (:Approx0	 #x0000)
  (:Approx1-1	 #x1000)
  (:Approx1-2	 #x2000)
  (:Approx1-N    #x4000)
  (:Approx1      #x7000))


(defun vector->array  (this len)
  (make-array len
	      :element-type 'single-float
	      :initial-contents (map 'list
				     #'number->single-float
				     (loop for i from 0 to (1- len)
					  collect (mem-aref this 'dReal i)))))

(defun vector3->array (this)
  (coerce (subseq this 0 3) '(SIMPLE-ARRAY SINGLE-FLOAT (3))))

(defun vector4->array (this)
  (coerce (subseq this 0 4) '(SIMPLE-ARRAY SINGLE-FLOAT (4))))

(defun matrix3->array (this)
  (coerce (subseq this 0 12) '(SIMPLE-ARRAY SINGLE-FLOAT (12))))

(defun matrix4->array (this)
  (coerce (subseq this 0 16) '(SIMPLE-ARRAY SINGLE-FLOAT (16))))

(defun matrix6->array (this)
  (coerce (subseq this 0 48) '(SIMPLE-ARRAY SINGLE-FLOAT (48))))


(defun array->vector  (this)
  (cffi:foreign-alloc 'dreal
		      :count (length this)
		      :initial-contents (loop for i from 0 to (1- (length this))
					     collect (aref this i))))
  
(defctype dVector3 (:array dReal 4))
(defctype dVector4 (:array dReal 4))
(defctype dMatrix3 (:array dReal 12))
(defctype dMatrix4 (:array dReal 16))
(defctype dMatrix6 (:array dReal 48))
(defctype dQuaternion (:array dReal 4))

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


(defcstruct dMass
  (mass dReal)
  (center dVector3)
  (inertia dMatrix3))

(define-condition ode-error (error)
  ((error-string :initarg :error-string :reader error-string))
  (:report (lambda (c stream)
	     (format stream "ode function returned error ~A"
		     (error-string c)))))

(defgeneric destroy (this))

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
       (setf (gethash (slot-value this 'pointer) *object-hash*) this))

     (defmethod translate-to-foreign ((val null) (type ,type-name))
       (null-pointer))

     (defmethod translate-to-foreign ((this ,name) (type ,type-name))
       (slot-value this 'pointer))
     
     (defmethod translate-from-foreign (pointer (type ,type-name))
       (unless (null-pointer-p pointer)
	   (make-instance ',name :pointer pointer)))

     (defmethod destroy ((this ,type-name))
       (declaim (ignore param))
       (remhash (slot-value this 'pointer) *object-hash*)
       (,(or
	  destructor
	  (intern (string-upcase (concatenate 'string "destroy-" (princ-to-string name))))) pointer)))))

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

(create-pointer-type world dWorldID )
(create-pointer-type pspace dSpaceID)
(create-pointer-type body dBodyID)
(create-pointer-type geometry dGeomID :destructor Geom-Destroy :superclass proto-geometry)
(create-pointer-type joint dJointID)
(create-pointer-type joint-group dJointGroupID :destructor Joint-Group-Destroy)


(create-pointer-subclass sphere dSphereID geometry dGeomID)
(create-pointer-subclass box dBoxID geometry dGeomID)
(create-pointer-subclass plane dPlaneID geometry dGeomID)
(create-pointer-subclass ray dRayID geometry dGeomID)
(create-pointer-subclass Contact-Joint dContactJointID joint dJointID)

(defcstruct dSurfaceParameters 
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

(defcstruct dContactGeom 
  (pos dVector3)
  (normal dVector3)
  (depth dReal)
  (g1 dGeomID)
  (g2 dGeomID)
  (side1 :int)
  (side2 :int))


(defcstruct dContact 
  (surface (:struct dSurfaceParameters))
  (geom (:struct dContactGeom))
  (fdir1 dVector3))
