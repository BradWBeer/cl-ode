(in-package :cl-ode)

(defgeneric world-get-gravity (this))

(defmethod world-get-gravity ((this world))
  (cffi:with-foreign-object (v 'dVector3)
    (dworldgetgravity this v)
    (vector->array v 3)))

(defgeneric world-set-defaults (this &key))

(defmethod world-set-defaults ((this world) &key)
  (world-set-gravity this 0 -6 0)
  (world-set-cfm this 1e-5)
  (world-set-damping this .001 .001)
  (world-set-linear-damping-threshold this 0.00001)
  (world-set-angular-damping-threshold this .005)
  (world-set-auto-disable-flag this 1))

