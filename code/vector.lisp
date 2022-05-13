(in-package #:cl-starpu)

(defstruct (vector
            (:include data)
            (:constructor %make-vector)
            (:constructor wrap-vector-handle (handle))))

(defun make-vector (length &rest args &key element-type initial-element)
  (declare (ignore initial-element))
  (multiple-value-bind (pointer foreign-type size)
      (apply #'starpu-allocate length args)
    (let* ((element-size (cffi:foreign-type-size foreign-type))
           (handle
             (cffi:with-foreign-object (handle :pointer)
               (%starpu-vector-data-register handle +starpu-main-ram+ pointer size element-size)
               (cffi:mem-ref handle :pointer))))
      (trivial-garbage:finalize
       (%make-vector
        :handle handle
        :element-type element-type
        :foreign-type foreign-type)
       (lambda ()
         (%starpu-data-unregister-no-coherency handle)
         (starpu-free pointer))))))

(defun vector-size (vector)
  (%starpu-vector-get-nx (vector-handle vector)))

(defun vector-pointer (vector)
  (%starpu-vector-get-local-ptr (vector-handle vector)))

(defmethod print-object ((vector vector) stream)
  (print-unreadable-object (vector stream :type t)
    (format stream "~@<~@{~S ~S~^ ~_~}~:>"
            :size (vector-size vector)
            :storage-vector (data-storage-vector vector))))
