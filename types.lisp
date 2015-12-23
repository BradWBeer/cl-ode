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
       (declaim (ignore param))
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



