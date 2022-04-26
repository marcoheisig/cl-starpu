(in-package #:cl-starpu-examples)

(cffi:defcallback hello-world :void ((buffers :pointer) (cl-arg :pointer))
  (declare (ignore buffers cl-arg))
  (format t "~&Hello world!~%"))

(defun hello ()
  (starpu-init)
  (starpu-task-insert
   (make-instance 'codelet
     :name "hello-world"
     :number-of-buffers 0
     :cpu-func-0 (cffi:callback hello-world))))
