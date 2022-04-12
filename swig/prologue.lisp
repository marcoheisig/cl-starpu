(in-package #:cl-starpu)

(cffi:define-foreign-library libstarpu
  (:unix (:or "libstarpu-1.3.so" "libstarpu-1.3.so.2"))
  (t (:default "libstarpu-1.3")))

(cffi:use-foreign-library libstarpu)

