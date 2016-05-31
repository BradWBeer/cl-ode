(in-package #:cl-ode)
(declaim (optimize (speed 3)))

(cffi:define-foreign-library libode
  (:unix (:or "libode.so" "libode.so.1" "libode.dylib"))
  (:windows "ode.dll")
  (t (:default "ode")))

(cffi:use-foreign-library libode)
