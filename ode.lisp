;;;; cl-ode.lisp
(in-package #:cl-ode)
(declaim (optimize (speed 3)))

(defvar *default-max-contacts* 25)  

(cffi:defcallback near-callback :void ((data :pointer)
				       (o1 :pointer)
				       (o2 :pointer))
  (declare (ignore data))
  (unless (pointer-eq o1 o2)

    
    (close-callback (gethash (cffi:pointer-address o1) *object-hash*)
		    (gethash (cffi:pointer-address o2) *object-hash*))))
		    

(cffi:defcallback moved-callback :void ((body :pointer))
  (let ((body (gethash (cffi:pointer-address body) *object-hash*)))
    (when (and body (move-handler body))
      (body-moved-callback body))))


(defcfun-rename-function ("dBodySetMovedCallback") :void 
  (body dBodyID)
  (callback :pointer))


(defun near-handler (data o1 o2)

  (unless (cffi:pointer-eq o1 o2)
    
    (let* ((lisp-object1 (gethash (pointer-address o1) *object-hash*))
	   (lisp-object2 (gethash (pointer-address o2) *object-hash*)))

      (when (and lisp-object1 lisp-object2)

	(close-callback lisp-object1 lisp-object2)))))


(defun init ()
  
  (init-ode)	      
  (setf *object-hash* (make-hash-table :test 'equal)))


(defun physics-step (world space)

  (space-collide space (null-pointer) (callback near-callback))
  (world-quick-step world (time-step world))
  (joint-group-empty (contact-group world)))

(defun uninit ()

  (close-ode)
  (setf *object-hash* nil))
  



