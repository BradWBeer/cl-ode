;; (defvar *physics-world*)
;; (defvar *physics-space*)
;; (defvar *physics-contact-group*)

;; (defun n->sf (x)
;;   (coerce x 'single-float))

;; (defun make-vector (a b c)
;;   (sb-cga:vec (coerce a 'single-float)
;; 	      (coerce b 'single-float)
;; 	      (coerce c 'single-float)))
 

;; (defmethod get-transform (position rotation)
;;   (sb-cga:matrix (n->sf (elt rotation 0)) (n->sf (elt rotation 1)) (n->sf (elt rotation 2))  (n->sf (elt position 0)) 
;; 		 (n->sf (elt rotation 4)) (n->sf (elt rotation 5)) (n->sf (elt rotation 6))  (n->sf (elt position 1))
;; 		 (n->sf (elt rotation 8)) (n->sf (elt rotation 9)) (n->sf (elt rotation 10)) (n->sf (elt position 2))
;; 		 (n->sf 0)                (n->sf 0)                (n->sf 0)                 (n->sf 1)))

;; (defun coerce-floats (val)
;;   (sb-cga:vec (coerce (aref val 0) 'single-float)
;; 	      (coerce (aref val 1) 'single-float)
;; 	      (coerce (aref val 2) 'single-float)))




;; (defmethod remove-vector (v1 v2)

;;   (let* ((fv1 (COERCE-FLOATS v1))
;; 	 (fv2 (COERCE-FLOATS v2))
;; 	 ;(n (sb-cga:normalize fv2))
;; 	 (dot (sb-cga:dot-product fv1 fv2))
;; 	 (ret (if (> dot 0)
;; 		  (sb-cga:vec- fv1 (sb-cga:vec* fv2 dot))
;; 		  v1)))

;;     ret))


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
  (typecase this
    (list (cffi:foreign-alloc 'dreal
			      :count (length this)
			      :initial-contents (loop for i in this
						   collect i)))
    (array
     (cffi:foreign-alloc 'dreal
			 :count (length this)
			 :initial-contents (loop for i from 0 to (1- (length this))
					      collect (aref this i))))))
