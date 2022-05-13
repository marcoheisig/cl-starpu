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
    (cffi:with-foreign-object (conf '(:struct %starpu-conf))
      (%starpu-conf-init conf)
      ;; Ensure that StarPU doesn't override our signal handlers.
      (setf (cffi:foreign-slot-value conf '(:struct %starpu-conf) '%catch-signals) 0)
      ;; Set the scheduler. TODO make this configurable
      (setf (cffi:foreign-slot-value conf '(:struct %starpu-conf) '%sched-policy-name)
            (cffi:foreign-string-alloc "eager"))
      ;; Process all keyword options.
      (macrolet ((process-unsigned-byte-option (variable slot)
                   `(etypecase ,variable
                      (null)            ; Keep the default.
                      (unsigned-byte
                       (setf (cffi:foreign-slot-value conf '(:struct %starpu-conf) ',slot)
                             ,variable))))
                 (process-boolean-option (variable slot)
                   `(progn
                      (check-type ,variable boolean)
                      (setf (cffi:foreign-slot-value conf '(:struct %starpu-conf) ',slot)
                            (if ,variable 1 0)))))
        (process-unsigned-byte-option ncpus %ncpus)
        (process-unsigned-byte-option reserve-ncpus %reserve-ncpus)
        (process-unsigned-byte-option ncuda %ncuda)
        (process-unsigned-byte-option nopencl %nopencl)
        (process-unsigned-byte-option nmic %nmic)
        (process-unsigned-byte-option nmpi-ms %nmpi-ms)
        (process-boolean-option bus-calibrate %bus-calibrate)
        (process-boolean-option calibrate %calibrate)
        (process-boolean-option single-combined-worker %single-combined-worker)
        (process-boolean-option disable-asynchronous-copy %disable-asynchronous-copy)
        (process-boolean-option disable-asynchronous-cuda-copy %disable-asynchronous-cuda-copy)
        (process-boolean-option disable-asynchronous-opencl-copy %disable-asynchronous-opencl-copy)
        (process-boolean-option disable-asynchronous-mic-copy %disable-asynchronous-mic-copy))
      ;; Actually initialize.
      (let ((ret (%starpu-init conf)))
        ;; Handle errors.
        (unless (zerop ret)
          (if (= ret (- +enodev+))
              (error 'no-worker-available :error-code ret)
              (error 'starpu-error :error-code ret)))))))

(defun shutdown ()
  (%starpu-shutdown))
