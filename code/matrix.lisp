(in-package #:cl-starpu)

(defstruct (matrix
            (:include data)
            (:constructor %make-matrix)))

(defun make-matrix
    (&key
       (nx (alexandria:required-argument :nx))
       (ny (alexandria:required-argument :ny))
       (ld nx)
       (element-type t)
       (initial-element nil initial-element-supplied-p))
  (let* ((array (apply #'make-pinned-array (list nx ny)
                       :element-type element-type
                       (when initial-element-supplied-p `(:initial-element ,initial-element))))
         (handle
           (cffi:with-foreign-object (handle :pointer)
             (%starpu-matrix-data-register
              handle
              +starpu-main-ram+
              (pinned-array-data-pointer array)
              ld nx ny
              (array-element-size array))
             (cffi:mem-ref handle :pointer))))
    (trivial-garbage:finalize
     (%make-matrix
      :handle handle
      :contents array)
     (lambda ()
       (%starpu-data-unregister-no-coherency handle)))))

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
            :ld (matrix-ld matrix)
            :contents (matrix-contents matrix))))
