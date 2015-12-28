(in-package :cl-ode)
(declaim (optimize (speed 3)))

(create-pointer-type pspace dSpaceID)
(create-pointer-subclass hash-space dHashSpaceID pspace dSpaceID)
(create-pointer-subclass quad-space dQuadSpaceID pspace dSpaceID)
