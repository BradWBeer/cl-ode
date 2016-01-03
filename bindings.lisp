(in-package #:cl-ode)
(declaim (optimize (speed 3)))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'swig-lispify-noprefix)
    (cl:defun swig-lispify-noprefix (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 (lower (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit 
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\_)
                       (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c))))
                  (strip-prefix (prf str)
                    (let ((l (length prf)))
                      (if (and (> (length str) l) (string= prf (subseq str 0 l)))
			  (subseq str l)
			  str))))
        (cl:let ((fix (cl:case flag
                        ((constant enumvalue) "+")
                        (variable "*")
                        (cl:t ""))))
          (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper (cl:concatenate 'cl:list (strip-prefix "d" name)) cl:nil cl:nil))
            fix)
           package))))))


(defmacro defcfun-rename-function ((name &optional new-name (export t)) &rest rest)
  (let ((lisp-name (or new-name
		       (swig-lispify-noprefix name 'function))))
    `(progn
       (defcfun (,name ,lisp-name)
	     ,@rest)
       ,(when export `(cl:export (swig-lispify-noprefix ,name 'function))))))


(defcfun-rename-function ("dGetConfiguration") :string)

(defcfun-rename-function ("dInitODE") :void)
(defcfun-rename-function ("dInitODE2") :void
  (init-flags :unsigned-int))

(defcfun-rename-function ("dCloseODE") :void)


(defcfun-rename-function ("dBodyCreate") dBodyID
  (world dWorldID))

(defcfun-rename-function ("dBodyGetWorld") dWorldID
  (body dBodyID))

(defcfun-rename-function ("dBodyDestroy") :void
  (world dBodyID))

(defcfun-rename-function ("dBodyGetPosition") dVector3
  (body dBodyID))

(defcfun-rename-function ("dBodyGetRotation") dMatrix3
  (body dBodyID))

(defcfun-rename-function ("dBodySetQuaternion") :void
  (body dBodyID)
  (q dQuaternion))

(defcfun-rename-function ("dBodyGetQuaternion") dQuaternion
  (body dBodyID))

(defcfun-rename-function ("dBodyGetMass" dbodygetmass) :void
  (body dBodyID)
  (mass dMass))

(defgeneric body-get-mass (this))
(defmethod body-get-mass ((this body))
  (let ((mass (make-instance 'ode::mass)))
    (dbodygetmass this mass)
    mass))
   
(defcfun-rename-function ("dBodySetMass") :void
  (body dBodyID)
  (mass dMass))

(defcfun-rename-function ("dBodySetPosition") :void
  (body dBodyID)
  (x dReal)
  (y dReal)
  (z dReal))

(defcfun-rename-function ("dBodySetLinearVel") :void
  (body dBodyID)
  (x dReal)
  (y dReal)
  (z dReal))


(defcfun-rename-function ("dBodySetAngularVel") :void
  (body dBodyID)
  (x dReal)
  (y dReal)
  (z dReal))

(defcfun-rename-function ("dBodyGetLinearVel") dVector3
  (body dBodyID))

(defcfun-rename-function ("dBodyGetAngularVel") dVector3
  (body dBodyID))

(defcfun-rename-function ("dBodySetMaxAngularSpeed") :void
  (body dBodyID)
  (max-speed dReal))

(defcfun-rename-function ("dBodySetRotation") :void
  (body dBodyID)
  (R dMatrix3))

(defcfun-rename-function ("dBodyEnable") :void
  (body dBodyID))

(defcfun-rename-function ("dBodyDisable") :void
  (body dBodyID))


(defcfun-rename-function ("dBodyIsEnabled") :boolean
  (body dBodyID))

(defcfun-rename-function ("dBodyGetFirstGeom") dGeomID
  (body dBodyID))

(defcfun-rename-function ("dBodyGetNextGeom") dGeomID
  (geom dGeomID))



(defcfun-rename-function ("dGeomSetCategoryBits") :void
  (geom dGeomID)
  (bits :unsigned-long))

(defcfun-rename-function ("dGeomSetCollideBits") :void
  (geom dGeomID)
  (bits :unsigned-long))

(defcfun-rename-function ("dGeomGetCollideBits") :unsigned-long
  (geom dGeomID))

(defcfun-rename-function ("dGeomGetCategoryBits") :unsigned-long
  (geom dGeomID))

(defcfun-rename-function ("dBodySetAutoDisableFlag") :void
  (body dBodyID)
  (auto-disable :int))

(defcfun-rename-function ("dBodyGetAutoDisableFlag") :int
  (body dBodyID))

(defcfun-rename-function ("dBodyAddForce") :void 
  (body dBodyID)
  (fx dReal)
  (fy dReal)
  (fz dReal))

(defcfun-rename-function ("dBodyAddTorque") :void
  (body dBodyID)
  (fx dReal)
  (fy dReal)
  (fz dReal))

(defcfun-rename-function ("dBodyAddRelForce") :void 
  (body dBodyID)
  (fx dReal)
  (fy dReal)
  (fz dReal))

(defcfun-rename-function ("dBodyAddRelTorque") :void
  (body dBodyID)
  (fx dReal)
  (fy dReal)
  (fz dReal))

(defcfun-rename-function ("dBodyAddForceAtPos") :void
  (body dBodyID)
  (fx dReal)
  (fy dReal) 
  (fz dReal)
  (px dReal)
  (py dReal)
  (pz dReal))

(defcfun-rename-function ("dBodyAddForceAtRelPos") :void
  (body dBodyID)
  (fx dReal)
  (fy dReal) 
  (fz dReal)
  (px dReal)
  (py dReal)
  (pz dReal))

(defcfun-rename-function ("dBodyAddRelForceAtPos") :void
  (body dBodyID)
  (fx dReal)
  (fy dReal) 
  (fz dReal)
  (px dReal)
  (py dReal)
  (pz dReal))

(defcfun-rename-function ("dBodyAddRelForceAtRelPos") :void
  (body dBodyID)
  (fx dReal)
  (fy dReal) 
  (fz dReal)
  (px dReal)
  (py dReal)
  (pz dReal))

(defcfun-rename-function ("dBodyGetForce") dVector3 
  (body dBodyID))

(defcfun-rename-function ("dBodyGetTorque") dVector3 
  (body dBodyID))

(defcfun-rename-function ("dBodySetForce") :void 
  (body dBodyID)
  (x dReal)
  (y dReal) 
  (z dReal))

(defcfun-rename-function ("dBodySetTorque") :void 
  (body dBodyID)
  (x dReal)
  (y dReal) 
  (z dReal))

(defcfun-rename-function ("dBodySetDynamic") :void 
  (body dBodyID))

(defcfun-rename-function ("dBodySetKinematic") :void 
  (body dBodyID))

(defcfun-rename-function ("dBodyIsKinematic") :boolean
  (body dBodyID))

(defcfun-rename-function ("dBodyVectorToWorld") :void
  (body dBodyID)
  (px dReal)
  (py dReal)
  (pz dReal)
  (result dVector3))

(defcfun-rename-function ("dBodyVectorFromWorld") :void
  (body dBodyID)
  (px dReal)
  (py dReal)
  (pz dReal)
  (result dVector3))

(defcfun-rename-function ("dBodySetAutoDisableLinearThreshold") :void 
  (body dBodyID)
  (linear-threshold dReal))

(defcfun-rename-function ("dBodyGetAutoDisableLinearThreshold") dReal
  (body dBodyID))

(defcfun-rename-function ("dBodySetAutoDisableAngularThreshold") :void  
  (body dBodyID) 
  (angular-threshold dReal))

(defcfun-rename-function ("dBodyGetAutoDisableAngularThreshold") dReal
  (body dBodyID))

(defcfun-rename-function ("dBodySetAutoDisableSteps") :void
  (body dBodyID)
  (steps :int))

(defcfun-rename-function ("dBodyGetAutoDisableSteps") :int
  (body dBodyID))

(defcfun-rename-function ("dBodySetAutoDisableTime") :void
  (body dBodyID)
  (time dReal))

(defcfun-rename-function ("dBodyGetAutoDisableTime") dReal
  (body dBodyID))

(defcfun-rename-function ("dBodySetAutoDisableAverageSamplesCount") :void 
  (body dBodyID)
  (average-samples-count :unsigned-int))

(defcfun-rename-function ("dBodyGetAutoDisableAverageSamplesCount") :int 
  (body dBodyID))

(defcfun-rename-function ("dBodySetAutoDisableDefaults") :void 
  (body dBodyID))

(defcfun ("dBodySetMovedCallback" Body-Set-Moved-Callback) :void 
  (body dBodyID)
  (callback :pointer))

(defcfun-rename-function ("dBodyGetLinearDamping") dReal
  (body dBodyID))

(defcfun-rename-function ("dBodyGetAngularDamping") dReal
  (body dBodyID))

(defcfun-rename-function ("dBodySetLinearDamping") :void
  (body dBodyID)
  (scale dReal)) 

(defcfun-rename-function ("dBodySetDamping") :void
  (body dBodyID)
  (scale dReal)) 


(defcfun-rename-function ("dBodySetAngularDamping") :void
  (body dBodyID)
  (scale dReal))

(defcfun-rename-function ("dGeomSetPosition") :void
  (geom dGeomID)
  (x dReal)
  (y dReal)
  (z dReal))

(defcfun-rename-function ("dGeomSetRotation") :void
  (geom dGeomID)
  (r dMatrix3))

(defcfun-rename-function ("dGeomSetQuaternion") :void
  (geom dGeomID)
  (q dQuaternion))

(defcfun-rename-function ("dGeomGetPosition") dVector3
  (geom dGeomID))

(defcfun-rename-function ("dGeomGetRotation") dMatrix3
  (geom dGeomID))

(defcfun-rename-function ("dGeomGetQuaternion") :void
  (geom dGeomID)
  (result dQuaternion))

(defcfun-rename-function ("dGeomSetOffsetPosition") :void
  (geom dGeomID)
  (x dReal)
  (y dReal)
  (z dReal))

(defcfun-rename-function ("dGeomSetOffsetRotation") :void
  (geom dGeomID)
  (r dMatrix3))

(defcfun-rename-function ("dGeomSetOffsetQuaternion") :void 
  (geom dGeomID)
  (q dQuaternion))

(defcfun-rename-function ("dGeomClearOffset") :void
  (geom dGeomID))

(defcfun-rename-function ("dCreateBox" Create-Box) dBoxID
  (space dSpaceID)
  (lx dReal)
  (ly dReal)
  (lz dReal))

(defcfun-rename-function ("dCreatePlane" Create-Plane) dPlaneID
  (space dSpaceID)
  (a dReal)
  (b dReal)
  (c dReal)
  (d dReal))

(defcfun-rename-function ("dGeomDestroy") :void
  (obj dGeomID))

(defcfun-rename-function ("dGeomSetBody") :void
  (geom dGeomID)
  (body dBodyID))

(defcfun-rename-function ("dHashSpaceCreate") dHashSpaceID
  (space dSpaceID))

(defcfun-rename-function ("dQuadTreeSpaceCreate") dQuadSpaceID
  (space dSpaceID)
  (center dVector3)
  (extents dVector3)
  (depth :int))

(defcfun-rename-function ("dJointGroupEmpty") :void
  (jointGroup dJointGroupID))

(defcfun-rename-function ("dJointAttach") :void
  (joint dJointID)
  (body1 dBodyID)
  (body2 dBodyID))

(defcfun-rename-function ("dJointGroupCreate") dJointGroupID
  (max-size :int))

(defcfun-rename-function ("dJointGroupDestroy") :void
  (joint-group dJointGroupID))

(defcfun-rename-function ("dMassSetBox") :void
  (mass dMass)
  (m dReal)
  (lx dReal)
  (ly dReal)
  (lz dReal))


(defcfun-rename-function ("dMassSetBoxTotal") :void
  (mass dMass)
  (m dReal)
  (lx dReal)
  (ly dReal)
  (lz dReal))


(defcfun-rename-function ("dMassSetZero") :void
  (mass dMass))

(defcfun-rename-function ("dRFromAxisAndAngle") :void
  (R dMatrix3)
  (ax dReal)
  (ay dReal)
  (az dReal)
  (angle dReal))

(defcfun-rename-function ("dSpaceCollide") :void
  (space dSpaceID)
  (data :pointer)
  (callback :pointer))

(defcfun-rename-function ("dWorldCreate") dWorldID) 

(defcfun-rename-function ("dWorldDestroy") :void
  (world dWorldID))

(defcfun-rename-function ("dWorldSetGravity") :void
  (world dWorldID)
  (x dReal)
  (y dReal)
  (z dReal))

(defcfun "dWorldGetGravity" :void
  (world dWorldID)
  (gravity :pointer))

(defcfun-rename-function ("dWorldStep") :void
  (world dWorldID)
  (step_size dReal))

(defcfun-rename-function ("dCreateSphere" Sphere-Create) dSphereID
  (space dSpaceID)
  (radius dReal))

(defcfun-rename-function ("dGeomSphereSetRadius") :void
  (sphere dSphereID)
  (radius dReal))

(defcfun-rename-function ("dGeomSphereGetRadius") dReal
  (sphere dSphereID))

(defcfun-rename-function ("dGeomSpherePointDepth") dReal
  (sphere dSphereID)
  (x dReal)
  (y dReal)
  (z dReal))
  
(defcfun-rename-function ("dMassSetSphere") :void
  (m dMass)
  (density dReal)
  (radius dReal))

;; (defgeneric body-set-sphere (this density radius))
;; (defmethod body-set-sphere ((this body) density radius)
;;   (with-foreign-object (mass '(:struct dmass))
;;     (mass-set-sphere mass density radius)
;;     (body-set-mass this mass)))


(defcfun-rename-function ("dMassSetSphereTotal") :void
  (m dMass)
  (total-mass dReal)
  (radius dReal))


(defcfun-rename-function ("dCreateCylinder" Cylinder-Create) dCylinderID
  (space dSpaceID)
  (radius dReal)
  (length dReal))

(defcfun-rename-function ("dMassSetCylinder") :void
  (m dMass)
  (density dReal)
  (direction :int)
  (radius dReal)
  (length dReal))

(defcfun-rename-function ("dMassSetCylinderTotal") :void
  (m dMass)
  (density dReal)
  (direction :int)
  (radius dReal)
  (length dReal))

(defcfun-rename-function ("dCreateCapsule" Capsule-Create) dCapsuleID
  (space dSpaceID)
  (radius dReal)
  (length dReal))

(defcfun-rename-function ("dMassSetCapsule") :void
  (m dMass)
  (density dReal)
  (direction :int)
  (radius dReal)
  (length dReal))

(defcfun-rename-function ("dMassSetCapsuleTotal") :void
  (m dMass)
  (density dReal)
  (direction :int)
  (radius dReal)
  (length dReal))

(defcfun-rename-function ("dCreateRay" Ray-Create) dRayID
  (space dSpaceID)
  (length dReal))

(defcfun-rename-function ("dGeomRaySetLength") :void 
  (ray dRayID)
  (length dReal))

(defcfun-rename-function ("dGeomRayGetLength") dReal
  (ray dRayID))

(defcfun-rename-function ("dGeomRaySet") :void 
  (ray dRayID)
  (px dReal)
  (py dReal)
  (pz dReal)
  (dx dReal)
  (dy dReal) 
  (dz dReal))

(defcfun-rename-function ("dGeomRayGet" dGeomRayGet) :void
  (ray dRayID)
  (start dVector3)
  (dir dVector3))

(defmethod geom-ray-get ((this ray))
  (cffi:with-foreign-objects ((start 'dVector3)
			      (dir 'dVector3))
    (dGeomRayGet this start dir)
    (values start
	    dir)))


(defcfun-rename-function ("dGeomRaySetParams") :void 
  (ray dRayID)
  (first-contact :int) 
  (backface-cull :boolean))

(defcfun ("dGeomRayGetParams" dGeomRayGetParams) :void 
  (ray dGeomID)
  (FirstContact (:pointer :int))
  (BackfaceCull (:pointer :int)))

(defmethod geom-ray-get-params ((this ray))
  (cffi:with-foreign-objects ((first-contact :int)
			      (back-face-cull :int))
    (dgeomraygetparams this first-contact back-face-cull)
    (values (cffi:mem-aref  first-contact :int)
	    (cffi:mem-aref  back-face-cull :int))))

(defcfun-rename-function ("dGeomRaySetClosestHit") :void
  (ray dRayID)
  (ClosestHit :int))

(defcfun-rename-function ("dGeomRayGetClosestHit") :int
  (ray dRayID))

(defcfun-rename-function ("dSpaceDestroy") :void
  (space dSpaceID))

(defcfun-rename-function ("dWorldSetCFM") :void
  (world dWorldID)
  (cfm dReal))

(defcfun-rename-function ("dWorldSetERP") :void
  (world dWorldID)
  (erp dReal))


(defcfun-rename-function ("dWorldQuickStep") :void
  (w dWorldID)
  (stepsize dReal))

(defcfun-rename-function ("dGeomGetBody") dBodyID
  (geom dGeomID))

(defcfun-rename-function ("dCollide") :int
  (o1 dGeomID)
  (o2 dGeomID)
  (flags :int)
  (contact :pointer)
  (skip :int))

(defcfun-rename-function ("dJointCreateContact") dContactJointID
  (world dWorldID)
  (joint-group dJointGroupID)
  (contact (:pointer dContact)))

(defcfun-rename-function ("dJointGetType") :int
  (id dJointID))

(defcfun-rename-function ("dWorldSetLinearDamping") :void
  (world dWorldID)
  (scale dReal))

(defcfun-rename-function ("dWorldSetAngularDamping") :void
  (world dWorldID)
  (scale dReal))

(defcfun-rename-function ("dWorldSetDamping") :void
  (world dWorldID)
  (linear-scale dReal)
  (angular-scale dReal))

(defcfun-rename-function ("dWorldGetLinearDampingThreshold") dReal
  (world dWorldID))

(defcfun-rename-function ("dWorldGetAngularDampingThreshold") dReal
  (world dWorldID))

(defcfun-rename-function ("dWorldSetLinearDampingThreshold") :void
  (world dWorldID)
  (threshold dReal))

(defcfun-rename-function ("dWorldSetAngularDampingThreshold") :void
  (world dWorldID) 
  (threshold dReal))

(defcfun-rename-function ("dWorldSetAutoDisableFlag") :void
  (world dWorldID)
  (auto-disable :int))

(defcfun-rename-function ("dWorldGetAutoDisableFlag") :boolean
  (world dWorldID))
