(in-package #:cl-ode)
(declaim (optimize (speed 3)))

(create-pointer-subclass ray dRayID geometry dGeomID)

(defmethod close-callback ((this ray) (that geometry))

  ;; no need for bodies here...
  (with-foreign-object (contact '(:struct Contact-struct) *default-max-contacts*)
    
    (let* ((geom (cffi:foreign-slot-pointer contact '(:struct ode::contact-struct) 'geom)))

      (let ((num-contacts (collide this
				   that
				   *default-max-contacts*
				   geom
				   (cffi:foreign-type-size '(:struct contact-struct)))))

	(unless (zerop num-contacts)

	  (values (cffi:foreign-slot-value  geom '(:struct contact-geometry-struct) 'depth)
		  (cffi:foreign-slot-value  geom '(:struct contact-geometry-struct) 'pos)
		  (cffi:foreign-slot-value  geom '(:struct contact-geometry-struct) 'normal)))))))

(defmethod close-callback ((this geometry) (that ray))
  (close-callback that this))

