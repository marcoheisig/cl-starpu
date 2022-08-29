(in-package #:cl-starpu)

;;; The CL-STARPU package shadows the symbols VECTOR and VECTORP so that we
;;; can keep StarPU's terminology.

(defstruct (vector
            (:include data)
            (:constructor %make-vector)))

(defun make-vector
    (&key
       (nx (alexandria:required-argument :nx))
       (element-type t)
       (initial-element nil initial-element-supplied-p))
  (let* ((array (apply #'make-pinned-array nx
                       :element-type element-type
                       (when initial-element-supplied-p `(:initial-element ,initial-element))))
         (handle
           (cffi:with-foreign-object (handle :pointer)
             (%starpu-vector-data-register
              handle
              +starpu-main-ram+
              (pinned-array-data-pointer array)
              nx
              (array-element-size array))
             (cffi:mem-ref handle :pointer))))
    (register-data-finalizer
     (%make-vector
      :handle handle
      :contents array))))

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
