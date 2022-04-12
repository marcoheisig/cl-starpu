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

(defun starpu-init
    (&key ncpus reserve-ncpus ncuda nopencl nmic nmpi-ms bus-calibrate calibrate
       single-combined-worker
       disable-asynchronous-copy
       disable-asynchronous-cuda-copy
       disable-asynchronous-opencl-copy
       disable-asynchronous-mic-copy)
  (unless (starpu-is-initialized)
    (cffi:with-foreign-object (conf '(:struct starpu-conf-cstruct))
      (%starpu-conf-init conf)
      ;; Ensure that StarPU doesn't override our signal handlers.
      (setf (cffi:foreign-slot-value conf '(:struct starpu-conf-cstruct) 'catch-signals-slot) 0)
      ;; Process all keyword options.
      (macrolet ((process-unsigned-byte-option (variable slot)
                   `(etypecase ,variable
                      (null)            ; Keep the default.
                      (unsigned-byte
                       (setf (cffi:foreign-slot-value conf '(:struct starpu-conf-cstruct) ',slot)
                             ,variable))))
                 (process-boolean-option (variable slot)
                   `(progn
                      (check-type ,variable boolean)
                      (setf (cffi:foreign-slot-value conf '(:struct starpu-conf-cstruct) ',slot)
                            (if ,variable 1 0)))))
        (process-unsigned-byte-option ncpus ncpus-slot)
        (process-unsigned-byte-option reserve-ncpus reserve-ncpus-slot)
        (process-unsigned-byte-option ncuda ncuda-slot)
        (process-unsigned-byte-option nopencl nopencl-slot)
        (process-unsigned-byte-option nmic nmic-slot)
        (process-unsigned-byte-option nmpi-ms nmpi-ms-slot)
        (process-boolean-option bus-calibrate bus-calibrate-slot)
        (process-boolean-option calibrate calibrate-slot)
        (process-boolean-option single-combined-worker single-combined-worker-slot)
        (process-boolean-option disable-asynchronous-copy disable-asynchronous-copy-slot)
        (process-boolean-option disable-asynchronous-cuda-copy disable-asynchronous-cuda-copy-slot)
        (process-boolean-option disable-asynchronous-opencl-copy disable-asynchronous-opencl-copy-slot)
        (process-boolean-option disable-asynchronous-mic-copy disable-asynchronous-mic-copy-slot))
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
      (%starpu-task-submit task))))
