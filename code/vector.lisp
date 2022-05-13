(in-package #:cl-starpu)

(defstruct (vector
            (:include data)
            (:constructor %make-vector)))

(defun make-vector (length &key element-type initial-element)
  (multiple-value-bind (pointer foreign-type size)
      (allocate length :element-type element-type :initial-element initial-element)
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
         (free pointer))))))

(defun vector-size (vector)
  (declare (vector vector))
  (%starpu-vector-get-nx (vector-handle vector)))

(defun vector-pointer (vector)
  (declare (vector vector))
  (%starpu-vector-get-local-ptr (vector-handle vector)))

(defun vector-nx (vector)
  (declare (vector vector))
  (%starpu-vector-get-nx (vector-handle vector)))

(defun vector-contents (vector)
  (declare (vector vector))
  (data-contents vector))

(defmethod print-object ((vector vector) stream)
  (print-unreadable-object (vector stream :type t)
    (format stream "~@<~@{~S ~S~^ ~_~}~:>"
            :nx (vector-nx vector)
            :contents (vector-contents vector))))
