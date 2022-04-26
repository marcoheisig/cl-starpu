(cl:in-package #:cl-user)

(defpackage #:cl-starpu
  (:nicknames #:starpu)
  (:use #:common-lisp)
  (:export
   #:starpu-is-initialized
   #:starpu-init
   #:starpu-pause
   #:starpu-resume
   #:starpu-shutdown
   #:unpack-arguments
   ;; Codelet
   #:codelet
   #:codeletp
   #:codelet-name
   #:codelet-type
   #:codelet-max-parallelism
   #:codelet-can-execute
   #:codelet-cpu-func
   #:codelet-cuda-func
   #:codelet-opencl-func
   #:codelet-performance-model
   #:codelet-energy-model
   #:codelet-number-of-buffers
   ;; Task
   #:task
   #:taskp
   #:task-name
   #:task-codelet
   #:task-codelet-arguments
   #:task-number-of-buffers
   #:starpu-task-insert))
