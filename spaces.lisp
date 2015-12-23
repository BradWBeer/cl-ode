(in-package :cl-ode)

(create-pointer-type pspace dSpaceID)
(create-pointer-subclass hash-space dHashSpaceID pspace dSpaceID)
(create-pointer-subclass quad-space dQuadSpaceID pspace dSpaceID)
