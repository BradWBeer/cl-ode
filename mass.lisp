(in-package #:cl-ode)


(defcstruct dMass-struct
  (mass dReal)
  (center dVector3)
  (inertia dMatrix3))

(define-foreign-type mass-type () ()
		     (:actual-type :pointer)
		     (:simple-parser dMass))

(defclass mass ()
  ((pointer :initform (foreign-alloc '(:struct dMass-struct))
	    :initarg :pointer)))

(defmethod initialize-instance ((this mass) &key)
  (setf (slot-value this 'pointer) (foreign-alloc '(:struct dmass-struct))))

(defmethod translate-to-foreign ((this mass) (type mass-type))
  (slot-value this 'pointer))

(defmethod translate-from-foreign (pointer (type mass-type))
       (unless (null-pointer-p pointer)
	   (make-instance 'mass :pointer pointer)))

;; (defmethod free-translated-object (pointer (type mass-type) param)
;;   (declare (ignore param))
;;   (when (delete? this) (foreign-free pointer)))


(defgeneric center (this))
(defmethod center ((this mass))
  (foreign-slot-value (slot-value this 'pointer) '(:struct dmass-struct) 'center))

(defmethod (setf center) (val (this mass))
  (setf (foreign-slot-value (slot-value this 'pointer) '(:struct dmass-struct) 'center)
	val))

(defgeneric mass (this))
(defmethod mass ((this mass))
  (foreign-slot-value (slot-value this 'pointer) '(:struct ode::dmass-struct) 'mass))

(defgeneric inertia (this))
(defmethod inertia ((this mass))
  (foreign-slot-value (slot-value this 'pointer) '(:struct ode::dmass-struct) 'inertia))

(defmethod (setf inertia) (val (this mass))
  (setf (foreign-slot-value (slot-value this 'pointer) '(:struct ode::dmass-struct) 'inertia)
 	val))

(defmethod destroy ((this mass))
  (foreign-free (slot-value this 'pointer)))
