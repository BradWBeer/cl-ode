(in-package #:cl-ode)

(defmacro combine-surface-properties (surface val1 val2 property)
  (let ((v1 (gensym))
	(v2 (gensym)))
    
    `(setf (foreign-slot-value ,surface '(:struct ode::dSurfaceParameters) ,property)
	   (let ((,v1 ,val1)
		 (,v2 ,val2))
	     (cond ((and ,v1 ,v2) (/ (+ ,v1 ,v2) 2))
		   ((and (null ,v1) ,v2) ,v2)
		   ((and (null ,v2) ,v1) ,v1)
		   (t 0))))))

