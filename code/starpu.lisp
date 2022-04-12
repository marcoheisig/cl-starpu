(in-package #:cl-starpu)

(defun starpu-is-initialized ()
  (not (zerop (%starpu-is-initialized))))

(defun starpu-pause ()
  (%starpu-pause))

(defun starpu-resume ()
  (%starpu-resume))

(defun starpu-version ()
  (cffi:with-foreign-objects ((major :int)
                              (minor :int)
                              (release :int))
    (%starpu-get-version major minor release)
    (values
     (cffi:mem-ref major :int)
     (cffi:mem-ref minor :int)
     (cffi:mem-ref release :int))))

(defun starpu-init (&key)
  (unless (starpu-is-initialized)
    (cffi:with-foreign-object (conf '(:struct starpu-conf-cstruct))
      (%starpu-conf-init conf)
      ;; Ensure that StarPU doesn't override our signal handlers.
      (setf (cffi:foreign-slot-value conf '(:struct starpu-conf-cstruct) 'catch-signals-slot) 0)
      ;; Actually initialize.
      (%starpu-init conf))))

(defun starpu-shutdown ()
  (%starpu-shutdown))

(defun starpu-display-stats ()
  (%starpu-display-stats))

(defun starpu-wait-initialized ()
  (%starpu-wait-initialized))

(cffi:defcallback hello :void ((buffers :pointer) (cl_arg :pointer))
  (declare (ignore buffers cl_arg))
  (format t "~&BAM!~%"))

(defun hello-world ()
  (starpu-init)
  (cffi:with-foreign-object (cl '(:struct starpu-codelet-cstruct))
    (setf (cffi:foreign-slot-value cl '(:struct starpu-codelet-cstruct) 'nbuffers-slot)
          0)
    (setf (cffi:mem-ref (cffi:foreign-slot-pointer cl '(:struct starpu-codelet-cstruct) 'cpu-funcs-slot)
                        :pointer 0)
          (cffi:callback hello))
    (let ((task (%starpu-task-create)))
      (setf (cffi:foreign-slot-value task '(:struct starpu-task-cstruct) 'cl-slot)
            cl)
      #+(or)
      (setf (cffi:foreign-slot-value task '(:struct starpu_task) 'synchronous)
            1)
      (%starpu-task-submit task))))
