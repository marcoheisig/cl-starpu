(in-package #:cl-starpu)

(defstruct (matrix
            (:include data)
            (:constructor %make-matrix)))

(defun make-matrix (nx ny &rest args &key element-type initial-element)
  (multiple-value-bind (pointer foreign-type)
      (starpu-allocate (list nx ny) :element-type element-type :initial-element initial-element)
    (let* ((element-size (cffi:foreign-type-size foreign-type))
           (handle
             (cffi:with-foreign-object (handle :pointer)
               (%starpu-matrix-data-register handle +starpu-main-ram+ pointer 0 nx ny element-size)
               (cffi:mem-ref handle :pointer))))
      (trivial-garbage:finalize
       (%make-matrix
        :handle handle
        :element-type element-type
        :foreign-type foreign-type)
       (lambda ()
         (%starpu-data-unregister-no-coherency handle)
         (starpu-free pointer))))))

(defun matrix-nx (matrix)
  (declare (matrix matrix))
  (%starpu-matrix-get-nx (matrix-handle matrix)))

(defun matrix-ny (matrix)
  (declare (matrix matrix))
  (%starpu-matrix-get-ny (matrix-handle matrix)))

(defun matrix-size (matrix)
  (declare (matrix matrix))
  (let ((handle (matrix-handle matrix)))
    (* (%starpu-matrix-get-nx handle)
       (%starpu-matrix-get-ny handle))))

(defun matrix-contents (matrix)
  (declare (matrix matrix))
  (make-array
   (list (matrix-nx matrix)
         (matrix-ny matrix))
   :element-type (matrix-element-type matrix)
   :displaced-to (data-storage-vector matrix)))

(defmethod print-object ((matrix matrix) stream)
  (print-unreadable-object (matrix stream :type t)
    (format stream "~@<~@{~S ~S~^ ~_~}~:>"
            :nx (matrix-nx matrix)
            :ny (matrix-ny matrix)
            :contents (matrix-contents matrix))))
