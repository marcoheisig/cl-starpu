(in-package #:cl-starpu)

(defclass vector-handle (data-handle)
  ((%vector
    :initarg :vector
    :type (simple-array * (*))
    :reader vector-handle-vector)))

(defun make-vector-handle (length &rest args &key element-type initial-element initial-contents)
  (declare (ignore initial-element initial-contents))
  (let* ((foreign-type (lisp-type-foreign-type element-type))
         (vector (apply #'static-vectors:make-static-vector length args))
         (ptr (static-vectors:static-vector-pointer vector))
         (nbytes (cffi:foreign-type-size foreign-type))
         (handle
           (cffi:with-foreign-object (handle-ptr :pointer)
             (%starpu-vector-data-register handle-ptr +starpu-main-ram+ ptr length nbytes)
             (cffi:mem-ref handle-ptr :pointer))))
    (assert (= (%starpu-vector-get-nx handle) length))
    (trivial-garbage:finalize
     (make-instance 'vector-handle
       :vector vector
       :handle handle)
     (lambda ()
       ;; TODO cleanup the data handle
       (static-vectors:free-static-vector vector)))))

(defun vector-handle-p (x)
  (typep x 'vector-handle))
