(cl:in-package #:cl-user)

(defpackage #:cl-starpu
  (:nicknames #:starpu)
  (:use #:common-lisp)
  ;; StarPU's terminology sometimes conflicts with that of Common Lisp, so
  ;; we have to shadow a few symbols.
  (:shadow #:vector #:vectorp #:block)
  (:export
   ;; load-foreign-code
   #:pkg-config
   #:load-foreign-code
   ;; starpu.lisp
   #:version
   #:initialize
   #:initializedp
   #:wait-initialized
   #:pause
   #:resume
   #:shutdown
   #:with-starpu-activity
   ;; memory-node.lisp
   #:memory-node
   #:memory-node-p
   #:memory-node-id
   #:cpu-memory-node
   #:cpu-memory-node-p
   #:cuda-memory-node
   #:cuda-memory-node-p
   #:opencl-memory-node
   #:opencl-memory-node-p
   #:disk-memory-node
   #:disk-memory-node-p
   #:mic-memory-node
   #:mic-memory-node-p
   #:mpi-ms-memory-node
   #:mpi-ms-memory-node-p
   #:*main-memory-node*
   ;; malloc.lisp
   #:malloc
   #:free
   #:make-pinned-array
   #:pinned-array-data-pointer
   #:array-element-size
   ;; worker.lisp
   #:worker
   #:workerp
   #:worker-id
   #:worker-memory-node
   #:cpu-worker
   #:cpu-workerp
   #:cuda-worker
   #:cuda-worker-p
   #:opencl-worker
   #:opencl-worker-p
   #:mic-worker
   #:mic-worker-p
   #:mpi-ms-worker
   #:mpi-ms-worker-p
   #:all-workers
   #:all-cpu-workers
   #:all-cuda-workers
   #:all-opencl-workers
   #:all-mic-workers
   #:all-mpi-ms-workers
   ;; data.lisp
   #:data
   #:datap
   #:data-foreign-type
   #:data-element-type
   #:data-bytes
   #:data-total-size
   #:data-local-pointer
   #:data-contents
   #:data-prefetch
   #:data-fetch
   #:data-acquire
   #:data-release
   #:data-unregister
   #:data-unregister-no-coherency
   #:with-acquired-data
   #:with-unpacked-arguments
   ;; vector.lisp
   #:make-vector
   #:vector
   #:vectorp
   #:vector-size
   #:vector-element-type
   #:vector-pointer
   #:vector-nx
   #:vector-contents
   ;; matrix.lisp
   #:make-matrix
   #:matrix
   #:matrixp
   #:matrix-size
   #:matrix-element-type
   #:matrix-pointer
   #:matrix-nx
   #:matrix-ny
   #:matrix-local-ld
   #:matrix-contents
   ;; block.lisp
   #:make-block
   #:block
   #:blockp
   #:block-size
   #:block-element-type
   #:block-pointer
   #:block-nx
   #:block-ny
   #:block-nz
   #:block-local-ldy
   #:block-local-ldz
   #:block-contents
   ;; codelet.lisp
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
   ;; task.lisp
   #:make-task
   #:task
   #:taskp
   #:task-name
   #:task-codelet
   #:task-codelet-arguments
   #:task-number-of-buffers
   #:task-insert
   #:task-wait
   #:task-wait-for-all))
