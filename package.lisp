;;;; package.lisp
(declaim (optimize (speed 3)))

(defpackage #:cl-ode
  (:use #:cl #:cffi)
  (:nicknames #:ode)
    (:export 

     #:dReal #:dMass #:dVector3 #:dVector4 #:dMatrix3 #:dMatrix4 #:dMatrix6
     #:dWorldID #:dSpaceID #:dBodyID #:dGeomID #:dJointID #:dJointGroupID
     #:dSurfaceParameters #:dContactGeom #:dContact
     #:set-transform
     #:*physics-world*
     #:*physics-space*
     #:*physics-contact-group
     #:*physics-max-contacts*
     #:*physics-geometry-hash*

     #:get-transform
     #:mode-options
     #:physics-mass
     #:unload
     #:physics-body
     #:physics-object
     #:physics-sphere
     #:physics-box
     #:physics-cylinder
     #:physics-capsule
     #:physics-plane
     #:init
     #:physics-step
     #:uninit
     #:body #:pointer
     #:enabled

     #:BOX-INDEXES #:BOX-VERTEXES #:BOX-NORMALS #:BOX-TEXCOORDS #:SPHERE-INDEXES
     #:SPHERE-VERTEXES #:SPHERE-NORMALS
     #:CYLINDER-INDEXES #:CYLINDER-VERTEXES #:CYLINDER-NORMALS
     #:ref #:unref
     #:set-position
     #:close-callback
     #:update
     #:ray-length #:physics-spring #:springiness #:damping
     #:horizontal #:vertical #:load-player-camera
     ))
