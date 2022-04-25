(in-package #:cl-starpu)

(cffi:defcallback hello :void ((buffers :pointer) (cl_arg :pointer))
  (declare (ignore buffers cl_arg))
  (format t "~&BAM!~%"))

(defun hello ()
  (starpu-init)
  (let ((codelet (make-instance 'codelet
                   :name "hello"
                   :number-of-buffers 0)))
    (setf (codelet-cpu-func codelet 0)
          (cffi:callback hello))
    (starpu-task-insert codelet)))
