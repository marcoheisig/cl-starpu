(in-package #:cl-starpu)

;;; The CL-STARPU package shadows the symbol BLOCK so that we can keep
;;; StarPU's terminology.

(defstruct (block
            (:include data)
            (:constructor %make-block)))

(defun make-displaced-block (array)
  (declare (array array))
  (let* ((nz (array-dimension array 0))
         (ny (array-dimension array 1))
         (nx (/ (array-total-size array) ny nz))
         (ldy nx)
         (ldz (* nx ny))
         (data (pinned-array-data-pointer array))
         (size (array-element-size array)))
    (register-data-finalizer
     (%make-block
      :contents array
      :handle
      (cffi:with-foreign-object (handle :pointer)
        (%starpu-block-data-register handle +starpu-main-ram+ data ldy ldz nx ny nz size)
        (cffi:mem-ref handle :pointer))))))

(defun make-block
    (&key
       (nx (alexandria:required-argument :nx))
       (ny (alexandria:required-argument :ny))
       (nz (alexandria:required-argument :nz))
       (ldy nx)
       (ldz (* nx ny))
       (size (alexandria:required-argument :size)))
  (register-data-finalizer
   (%make-block
    :handle
    (cffi:with-foreign-object (handle :pointer)
      (%starpu-block-data-register handle -1 (cffi:null-pointer) ldy ldz nx ny nz size)
      (cffi:mem-ref handle :pointer)))))

(defun block-local-ldy (block)
  (declare (block block))
  (%starpu-block-get-local-ldy (block-handle block)))

(defun block-local-ldz (block)
  (declare (block block))
  (%starpu-block-get-local-ldz (block-handle block)))

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

(defmethod print-object ((block block) stream)
  (print-unreadable-object (block stream :type t)
    (format stream "~@<~@{~S ~S~^ ~_~}~:>"
            :nx (block-nx block)
            :ny (block-ny block)
            :nz (block-nz block)
            :ldy (block-local-ldy block)
            :ldz (block-local-ldz block)
            :contents (block-contents block))))
