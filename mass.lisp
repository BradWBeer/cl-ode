(in-package #:cl-ode)


;; (define-foreign-type mass-type () ()
;; 		     (:actual-type :pointer)
;; 		     (:simple-parser dMass))

;; (defclass mass ()
;;   ((pointer :initform (foreign-alloc '(:struct dMass-struct))
;; 	    :initarg :pointer)))

;; (defmethod initialize-instance ((this mass) &key)
;;   (setf (slot-value this 'pointer) (foreign-alloc '(:struct dmass-struct))))

;; (defmethod translate-to-foreign ((this mass) (type mass-type))
;;   (slot-value this 'pointer))

;; (defmethod translate-from-foreign (pointer (type mass-type))
;;        (unless (null-pointer-p pointer)
;; 	   (make-instance 'mass :pointer pointer)))

;; ;; (defmethod free-translated-object (pointer (type mass-type) param)
;; ;;   (declare (ignore param))
;; ;;   (when (delete? this) (foreign-free pointer)))


;; (defmacro struct-slot (name type slot)
;;   `(defmacro ,name (this) 
;;        `(foreign-slot-value (slot-value ,this 'pointer)
;; 		      ',,type ',,slot)))

;; (struct-slot mass (:struct dmass-struct) 'mass)
;; (struct-slot center (:struct dmass-struct) 'center)
;; (struct-slot inertia (:struct dmass-struct) 'inertia)

