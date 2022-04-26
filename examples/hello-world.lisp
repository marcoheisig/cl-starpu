(in-package #:cl-starpu)

(cffi:defcallback hello-world :void ((buffers :pointer) (cl_arg :pointer))
  (declare (ignore buffers cl_arg))
  (format t "~&Hello world!~%"))

(defun hello ()
  (starpu-init)
  (starpu-task-insert
   (make-instance 'codelet
     :name "hello-world"
     :number-of-buffers 0
     :cpu-func-0 (cffi:callback hello-world))))
