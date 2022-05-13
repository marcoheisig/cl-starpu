(in-package #:cl-starpu)

(defstruct (block
            (:include data)
            (:constructor %make-block)))

(defun make-block (nx ny nz &rest args &key element-type initial-element)
  (multiple-value-bind (pointer foreign-type)
      (allocate (list nx ny nz) :element-type element-type :initial-element initial-element)
    (let* ((element-size (cffi:foreign-type-size foreign-type))
           (handle
             (cffi:with-foreign-object (handle :pointer)
               (%starpu-block-data-register handle +starpu-main-ram+ pointer 0 0 nx ny nz element-size)
               (cffi:mem-ref handle :pointer))))
      (trivial-garbage:finalize
       (%make-block
        :handle handle
        :element-type element-type
        :foreign-type foreign-type)
       (lambda ()
         (%starpu-data-unregister-no-coherency handle)
         (free pointer))))))

(defun block-nx (block)
  (declare (block block))
  (%starpu-block-get-nx (block-handle block)))

(defun block-ny (block)
  (declare (block block))
  (%starpu-block-get-ny (block-handle block)))

(defun block-nz (block)
  (declare (block block))
  (%starpu-block-get-nz (block-handle block)))

(defun block-size (block)
  (declare (block block))
  (let ((handle (block-handle block)))
    (* (%starpu-block-get-nx handle)
       (%starpu-block-get-ny handle)
       (%starpu-block-get-nz handle))))

(defun block-contents (block)
  (declare (block block))
  (make-array
   (list (block-nx block)
         (block-ny block)
         (block-nz block))
   :element-type (block-element-type block)
   :displaced-to (data-storage-vector block)))

(defmethod print-object ((block block) stream)
  (print-unreadable-object (block stream :type t)
    (format stream "~@<~@{~S ~S~^ ~_~}~:>"
            :nx (block-nx block)
            :ny (block-ny block)
            :nz (block-nz block)
            :contents (block-contents block))))
