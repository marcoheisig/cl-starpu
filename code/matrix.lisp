(in-package #:cl-starpu)

(defstruct (matrix
            (:include data)
            (:constructor %make-matrix)))

(defun make-displaced-matrix (array)
  (declare (array array))
  (let* ((ny (array-dimension array 0))
         (nx (array-dimension array 1))
         (ld nx)
         (data (pinned-array-data-pointer array))
         (size (array-element-size array)))
    (register-data-finalizer
     (%make-matrix
      :contents array
      :handle
      (cffi:with-foreign-object (handle :pointer)
        (%starpu-matrix-data-register handle +starpu-main-ram+ data ld nx ny size)
        (cffi:mem-ref handle :pointer))))))

(defun make-matrix
    (&key
       (nx (alexandria:required-argument :nx))
       (ny (alexandria:required-argument :ny))
       (ld nx)
       (size (alexandria:required-argument :size)))
  (register-data-finalizer
   (%make-matrix
    :handle
    (cffi:with-foreign-object (handle :pointer)
      (%starpu-matrix-data-register handle -1 (cffi:null-pointer) ld nx ny size)
      (cffi:mem-ref handle :pointer)))))

(defun matrix-nx (matrix)
  (declare (matrix matrix))
  (%starpu-matrix-get-nx (matrix-handle matrix)))

(defun matrix-ny (matrix)
  (declare (matrix matrix))
  (%starpu-matrix-get-ny (matrix-handle matrix)))

(defun matrix-local-ld (matrix)
  (declare (matrix matrix))
  (%starpu-matrix-get-local-ld (matrix-handle matrix)))

(defun matrix-size (matrix)
  (declare (matrix matrix))
  (let ((handle (matrix-handle matrix)))
    (* (%starpu-matrix-get-nx handle)
       (%starpu-matrix-get-ny handle))))

(defmethod print-object ((matrix matrix) stream)
  (print-unreadable-object (matrix stream :type t)
    (format stream "~@<~@{~S ~S~^ ~_~}~:>"
            :nx (matrix-nx matrix)
            :ny (matrix-ny matrix)
            :ld (matrix-local-ld matrix)
            :contents (matrix-contents matrix))))
