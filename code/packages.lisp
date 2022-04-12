(cl:in-package #:cl-user)

(defpackage #:cl-starpu
  (:nicknames #:starpu)
  (:use #:common-lisp)
  (:export
   #:starpu-is-initialized
   #:starpu-init
   #:starpu-pause
   #:starpu-resume
   #:starpu-shutdown))
