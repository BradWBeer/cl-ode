(in-package :cl-ode)
(declaim (optimize (speed 3)))

(create-pointer-type joint dJointID)
(create-pointer-type joint-group dJointGroupID :destructor Joint-Group-Destroy)
(create-pointer-subclass Contact-Joint dContactJointID joint dJointID)
