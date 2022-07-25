(in-package #:cl-starpu-examples)

;;; This example demonstrates how scalar arguments can be passed to a
;;; codelet.

(cffi:defcallback codelet-arguments :void ((buffers :pointer) (cl-arg :pointer))
  (declare (ignore buffers))
  (starpu:with-unpacked-arguments cl-arg ((a :int) (b :double) (c :float))
    (format t "~&a: ~S~%b: ~S~%c: ~S~%" a b c)))

(defun codelet-arguments ()
  (starpu:initialize)
  (starpu:task-insert
   (starpu:make-codelet
    :name 'codelet-arguments
    :cpu-func-0 (cffi:callback codelet-arguments))
   :int 42
   :double (coerce pi 'double-float)
   :float 2f0))
