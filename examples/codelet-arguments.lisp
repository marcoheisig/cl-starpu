(in-package #:cl-starpu-examples)

(cffi:defcallback codelet-arguments :void ((buffers :pointer) (cl-arg :pointer))
  (declare (ignore buffers))
  (multiple-value-bind (a b c) (unpack-arguments cl-arg :int :float :double)
    (format t "~&a: ~S~%b: ~S~%c: ~S~%" a b c)))

(defun codelet-arguments ()
  (starpu-init)
  (starpu-task-insert
   (make-instance 'codelet
     :name "codelet-arguments"
     :number-of-buffers 0
     :cpu-func-0 (cffi:callback codelet-arguments))
   :int 42
   :float 2f0
   :double (coerce pi 'double-float)))
