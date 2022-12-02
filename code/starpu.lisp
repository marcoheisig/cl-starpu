(in-package #:cl-starpu)

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

(defun initialize
    (&key ncpus reserve-ncpus ncuda nopencl nmpi-ms bus-calibrate calibrate
       single-combined-worker
       disable-asynchronous-copy
       disable-asynchronous-cuda-copy
       disable-asynchronous-opencl-copy)
  (unless (initializedp)
    (cffi:with-foreign-object (conf '(:struct %starpu-conf))
      (%starpu-conf-init conf)
      ;; Ensure that StarPU doesn't override our signal handlers.
      (setf (cffi:foreign-slot-value conf '(:struct %starpu-conf) '%catch-signals) 0)
      ;; Set the scheduler. TODO make this configurable
      (setf (cffi:foreign-slot-value conf '(:struct %starpu-conf) '%sched-policy-name)
            (cffi:foreign-string-alloc "dmdas"))
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
        (process-unsigned-byte-option nmpi-ms %nmpi-ms)
        (process-boolean-option bus-calibrate %bus-calibrate)
        (process-boolean-option calibrate %calibrate)
        (process-boolean-option single-combined-worker %single-combined-worker)
        (process-boolean-option disable-asynchronous-copy %disable-asynchronous-copy)
        (process-boolean-option disable-asynchronous-cuda-copy %disable-asynchronous-cuda-copy)
        (process-boolean-option disable-asynchronous-opencl-copy %disable-asynchronous-opencl-copy))
      ;; Actually initialize.
      (let ((ret (%starpu-init conf)))
        ;; Handle errors.
        (unless (zerop ret)
          (if (= ret (- +enodev+))
              (error 'no-worker-available :error-code ret)
              (error 'starpu-error :error-code ret)))
        ;; Initialize cl-starpu.
        (setf *main-memory-node* (%make-memory-node +starpu-main-ram+))
        (values)))))

(defun shutdown ()
  (%starpu-shutdown))

(defun wait-initialized ()
  (%starpu-wait-initialized))

(let ((lock (bordeaux-threads:make-lock "StarPU Activity Lock"))
      (count 1))
  (defun pause ()
    "Decrement the number of StarPU users by one.  If the number of users
reaches zero, pause all StarPU activity."
    (bordeaux-threads:with-lock-held (lock)
      (case (decf count)
        (0 (%starpu-pause))
        (-1 (error "Must not pause StarPU more than once.")))))

  (defun resume ()
    "Resume StarPU activity."
    (bordeaux-threads:with-lock-held (lock)
      (when (= 1 (incf count))
        (%starpu-resume)))))

(defmacro with-starpu-activity (&body body)
  "Ensure StarPU is not paused while evaluating BODY."
  `(call-with-starpu-activity (lambda () ,@body)))

(defun call-with-starpu-activity (thunk)
  (resume)
  (unwind-protect (funcall thunk)
    (pause)))
