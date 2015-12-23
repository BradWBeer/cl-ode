(in-package #:cl-ode)

(create-pointer-type body dBodyID)

(defmethod initialize-instance :after ((this body) &key)
  (body-set-moved-callback this (callback moved-callback)))

(defmethod body-get-transform ((this body))
  (let ((position (body-get-position this))
	(rotation (body-get-rotation this)))
    (make-array 16
		:element-type 'single-float
		:initial-contents (list (elt rotation 0) (elt rotation 4) (elt rotation 8)  0.0
					(elt rotation 1) (elt rotation 5) (elt rotation 9)  0.0
					(elt rotation 2) (elt rotation 6) (elt rotation 10) 0.0
					(elt position 0) (elt position 1) (elt position 2)  1.0))))


(defmethod body-set-transform ((this body) (m array))

  (body-set-position this (elt m 12) (elt m 13) (elt m 14)))

(defmethod body-moved-callback ((this body) &key)

  (format t "Body-Moved-Callback ~A: ~A~%" this (body-get-position this)))

(defmethod body-get-geoms ((this body) &key)

  (let ((first-body (body-get-first-geom this)))
    (when first-body
      (cons first-body
	    (loop
	       for b = (body-get-next-geom (or b first-body))
	       while b
	       collect b)))))
