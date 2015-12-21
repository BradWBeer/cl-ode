(in-package #:cl-ode)

(cffi:define-foreign-library libode
  (:unix (:or "libode.so" "libode.dylib"))
  (:windows "ode.dll")
  (t (:default "ode")))

(cffi:use-foreign-library libode)
