(in-package #:cl-ode)
(declaim (optimize (speed 3)))

(defun make-vector3 (x y z w)
  (make-array 4 :element-type 'single-float :initial-contents (list x y z w)))

(defun make-zero-vector3 ()
  (make-array 4 :element-type 'single-float :initial-element 0.0))

(defun make-zero-vector4 ()
  (make-array 4 :element-type 'single-float :initial-element 0.0))

(defun multiply-vector (s v)
  (map '(SIMPLE-ARRAY SINGLE-FLOAT) (lambda (x)
				      (* s x))
       v))

(defun add-vectors (v1 v2)
  (map '(SIMPLE-ARRAY SINGLE-FLOAT) #'- v1 v2))

(defun subtract-vectors (v1 v2)
  (map '(SIMPLE-ARRAY SINGLE-FLOAT) #'- v1 v2))

(defun dot (v1 v2)
  (map '(SIMPLE-ARRAY SINGLE-FLOAT) #'* v1 v2))

(defmacro with-ode-object ((name value) &body body)
  (let ((hidden-name (gensym)))
    `(let* ((,hidden-name ,value)
	    (,name ,hidden-name))
       (unwind-protect
	    (progn 
	      ,@body)	      
	 (destroy ,hidden-name)))))
