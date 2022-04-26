(in-package #:cl-starpu)

(defvar *ctx*) ; TODO

(defun version ()
  (cffi:with-foreign-objects ((major :int)
                              (minor :int)
                              (release :int))
    (%starpu-get-version major minor release)
    (values
     (cffi:mem-ref major :int)
     (cffi:mem-ref minor :int)
     (cffi:mem-ref release :int))))

(defun initializedp ()
  (not (zerop (%starpu-is-initialized))))

(defun pause ()
  (%starpu-pause))

(defun resume ()
  (%starpu-resume))

(defun wait-initialized ()
  (%starpu-wait-initialized))

(defun initialize
    (&key ncpus reserve-ncpus ncuda nopencl nmic nmpi-ms bus-calibrate calibrate
       single-combined-worker
       disable-asynchronous-copy
       disable-asynchronous-cuda-copy
       disable-asynchronous-opencl-copy
       disable-asynchronous-mic-copy)
  (unless (initializedp)
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
      (let ((ret (%starpu-init conf)))
        ;; Handle errors.
        (unless (zerop ret)
          (if (= ret (- +enodev+))
              (error 'no-worker-available :error-code ret)
              (error 'starpu-error :error-code ret)))))))

(defun shutdown ()
  (%starpu-shutdown))
