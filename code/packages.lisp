(cl:in-package #:cl-user)

(defpackage #:cl-starpu
  (:nicknames #:starpu)
  (:use #:common-lisp)
  (:export
   #:version
   #:initialize
   #:initializedp
   #:wait-initialized
   #:pause
   #:resume
   #:shutdown
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
   #:task-insert
   #:task-wait
   #:task-wait-for-all
   #:unpack-arguments
   ))
