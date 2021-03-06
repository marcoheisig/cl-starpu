(in-package #:cl-starpu-examples)

;;; This example demonstrates how scalar arguments can be passed to a
;;; codelet.

(cffi:defcallback codelet-arguments :void ((buffers :pointer) (cl-arg :pointer))
  (declare (ignore buffers))
  (starpu:with-unpacked-arguments cl-arg ((a :int) (b :float) (c :double))
    (format t "~&a: ~S~%b: ~S~%c: ~S~%" a b c)))

(defun codelet-arguments ()
  (starpu:initialize)
  (starpu:task-insert
   (make-instance 'starpu:codelet
     :name "codelet-arguments"
     :cpu-func-0 (cffi:callback codelet-arguments))
   :int 42
   :float 2f0
   :double (coerce pi 'double-float)))
