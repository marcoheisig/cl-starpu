(in-package #:cl-starpu)

;;; The CL-STARPU package shadows the symbol BLOCK so that we can keep
;;; StarPU's terminology.

(defstruct (block
            (:include data)
            (:constructor %make-block)))

(defun make-block
    (&key
       (nx (alexandria:required-argument :nx))
       (ny (alexandria:required-argument :ny))
       (nz (alexandria:required-argument :nz))
       (ldy nx)
       (ldz (* nx ny))
       (element-type t)
       (initial-element nil initial-element-supplied-p))
  (let* ((array (apply #'make-pinned-array (list nx ny nz)
                       :element-type element-type
                       (when initial-element-supplied-p `(:initial-element ,initial-element))))
         (handle
           (cffi:with-foreign-object (handle :pointer)
             (%starpu-block-data-register
              handle
              +starpu-main-ram+
              (pinned-array-data-pointer array)
              ldy ldz nx ny nz
              (array-element-size array))
             (cffi:mem-ref handle :pointer))))
    (register-data-finalizer
     (%make-block
      :handle handle
      :contents array))))

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
