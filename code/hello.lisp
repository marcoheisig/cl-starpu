(in-package #:cl-starpu)

(cffi:defcallback hello :void ((buffers :pointer) (cl_arg :pointer))
  (declare (ignore buffers cl_arg))
  (format t "~&BAM!~%"))

(defun hello-world ()
  (starpu-init)
  (let ((codelet (make-instance 'sequential-codelet
                   :name "hello"
                   :number-of-buffers 0)))
    (setf (codelet-cpu-func codelet 0)
          (cffi:callback hello))
    (let ((task (%starpu-task-create)))
      (setf (cffi:foreign-slot-value task '(:struct starpu-task-cstruct) 'cl-slot)
            (codelet-handle codelet))
      (%starpu-task-submit task))))
