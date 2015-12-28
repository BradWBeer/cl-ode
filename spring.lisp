(in-package #:cl-ode)
(declaim (optimize (speed 3)))

(defclass spring (ray)
  ((springiness :initform 1
		:initarg :springiness
		:accessor springiness)
   (damping :initform .1
	    :initarg :damping
	    :accessor damping)))

(defmethod close-callback :before ((this spring) (that geometry))

  (let* ((b1 (geom-get-body this))
	 (b2 (geom-get-body that)))

    (when (or b1 b2)
      (multiple-value-bind (depth position normal) (call-next-method) (declare (ignore normal position))

	(multiple-value-bind (start direction) (Geom-Ray-Get this) (declare (ignore start))

	  (let* ((v1 (if b1 (body-get-linear-vel b1) (make-zero-vector3)))
		 (v2 (if b2 (body-get-linear-vel b2) (make-zero-vector3)))
		 (speed (dot v1 v2))
		 (len (geom-ray-get-length this))
		 (x  (+ (* (- 1 (damping this)) speed)
			(* (springiness this) (- depth len))))
		 (force (multiply-vector x direction)))

	    (when b1
	      (body-add-force b1 (aref force 0) (aref force 1) (aref force 2)))

	    (when b2
	      (body-add-force b2 (- (aref force 0)) (- (aref force 1)) (- (aref force 2))))))))))


(defmethod close-callback ((this geometry) (that ray))
  (close-callback that this))
