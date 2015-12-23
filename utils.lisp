(in-package #:cl-ode)


(defmacro with-ode-object ((name value) &body body)
  (let ((hidden-name (gensym)))
    `(let* ((,hidden-name ,value)
	    (,name ,hidden-name))
       (unwind-protect
	    (progn 
	      ,@body)	      
	 (destroy ,hidden-name)))))
