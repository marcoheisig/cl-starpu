(in-package :cl-starpu)

(defun malloc (n)
  (cffi:with-foreign-object (pointer :pointer)
    (let ((err (%starpu-malloc pointer n)))
      (unless (zerop err)
        (error "Call to (malloc ~D) failed with exit code ~D." n err)))
    (cffi:mem-ref pointer :pointer)))

(defun free (pointer)
  (%starpu-free pointer))

#+sbcl
(defun pinned-array-data-pointer (array)
  (static-vectors:static-vector-pointer
   (sb-vm::%data-vector-and-index array 0)))

#-sbcl
(defun pinned-array-data-pointer (array)
  (static-vectors:static-vector-pointer
   (break "TODO")))

#+sbcl
(let ((cache (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
  (loop for saetp across sb-vm:*specialized-array-element-type-properties* do
    (let* ((array (make-array 0 :element-type (sb-vm:saetp-specifier saetp)))
           (index (- (sb-kernel:array-underlying-widetag array) 128)))
      (setf (aref cache index)
            (ceiling (sb-vm:saetp-n-bits saetp) sb-vm:n-byte-bits))))
  (defun array-element-size (array)
    (declare (array array))
    (aref cache (- (sb-kernel:array-underlying-widetag array) 128))))

#-sbcl
(defun array-element-size (array)
  (cffi:foreign-type-size
   (lisp-type-foreign-type
    (array-element-type array))))
