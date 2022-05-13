(cl:in-package #:cl-user)

(defpackage #:cl-starpu
  (:nicknames #:starpu)
  (:use #:common-lisp)
  (:shadow #:vector #:vectorp #:block)
  (:export
   #:version
   #:initialize
   #:initializedp
   #:wait-initialized
   #:pause
   #:resume
   #:shutdown
   #:with-unpacked-arguments
   #:with-unpacked-buffers
   ;; Data
   #:data
   #:datap
   ;; Vector
   #:make-vector
   #:vector
   #:vectorp
   #:vector-nx
   #:vector-ptr
   #:vector-elemsize
   ;; Codelet
   #:make-codelet
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
   #:codelet-nbuffers
   #:codelet-mode
   #:codelet-modes
   ;; Task
   #:make-task
   #:task
   #:taskp
   #:task-name
   #:task-codelet
   #:task-codelet-arguments
   #:task-number-of-buffers
   #:task-insert
   #:task-wait
   #:task-wait-for-all
   ))
