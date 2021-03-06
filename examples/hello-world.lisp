(in-package #:cl-starpu-examples)

;;; This example demonstrates how a simple codelet can be created and
;;; scheduled via StarPU.

(cffi:defcallback hello-world :void ((buffers :pointer) (cl-arg :pointer))
  (declare (ignore buffers cl-arg))
  (format t "~&Hello world!~%"))

(defun hello-world ()
  (starpu:initialize)
  (starpu:task-insert
   (make-instance 'starpu:codelet
     :name "hello-world"
     :cpu-func-0 (cffi:callback hello-world))))
