(in-package #:cl-starpu)

;;; The CL-STARPU package shadows the symbols VECTOR and VECTORP so that we
;;; can keep StarPU's terminology.

(defstruct (vector
            (:include data)
            (:constructor %make-vector)))

(defun make-displaced-vector (array)
  (declare (array array))
  (let* ((nx (array-total-size array))
         (data (pinned-array-data-pointer array))
         (size (array-element-size array)))
    (register-data-finalizer
     (%make-vector
      :contents array
      :handle
      (cffi:with-foreign-object (handle :pointer)
        (%starpu-vector-data-register handle +starpu-main-ram+ data nx size)
        (cffi:mem-ref handle :pointer))))))

(defun make-vector
    (&key
       (nx (alexandria:required-argument :nx))
       (size (alexandria:required-argument :size)))
  (register-data-finalizer
   (%make-vector
    :handle
    (cffi:with-foreign-object (handle :pointer)
      (%starpu-vector-data-register handle -1 (cffi:null-pointer) nx size)
      (cffi:mem-ref handle :pointer)))))

(defun vector-nx (vector)
  (declare (vector vector))
  (%starpu-vector-get-nx (vector-handle vector)))

(defun vector-size (vector)
  (declare (vector vector))
  (vector-nx vector))

(defmethod print-object ((vector vector) stream)
  (print-unreadable-object (vector stream :type t)
    (format stream "~@<~@{~S ~S~^ ~_~}~:>"
            :nx (vector-nx vector)
            :contents (vector-contents vector))))
