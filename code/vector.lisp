(in-package #:cl-starpu)

(defstruct (vector
            (:include interface)
            (:constructor %make-vector (handle static-vector))
            (:constructor wrap-vector-handle (handle)))
  (static-vector nil
   :type (or common-lisp:vector null)))

(defun make-vector (length &rest args &key element-type initial-element initial-contents)
  (declare (ignore element-type initial-element initial-contents))
  (let* ((static-vector (apply #'static-vectors:make-static-vector length args))
         (ptr (static-vectors:static-vector-pointer static-vector))
         (foreign-type (lisp-type-foreign-type (array-element-type static-vector)))
         (size (cffi:foreign-type-size foreign-type))
         (handle
           (cffi:with-foreign-object (handle :pointer)
             (%starpu-vector-data-register handle +starpu-main-ram+ ptr length size)
             (cffi:mem-ref handle :pointer))))
    (trivial-garbage:finalize
     (%make-vector handle static-vector)
     (lambda ()
       (%starpu-data-unregister-no-coherency handle)
       (static-vectors:free-static-vector static-vector)))))

(defun vector-nx (vector)
  (%starpu-vector-get-nx (vector-handle vector)))

(defun vector-ptr (vector)
  (%starpu-vector-get-local-ptr (vector-handle vector)))

(defun vector-elemsize (vector)
  (%starpu-vector-get-elemsize (vector-handle vector)))

(defun vector-data (vector)
  (vector-static-vector vector))

(defmethod print-object ((vector vector) stream)
  (print-unreadable-object (vector stream :type t)
    (format stream "~@<~@{~S ~S~^ ~_~}~:>"
            :nx (vector-nx vector)
            :elemsize (vector-elemsize vector)
            :static-vector (vector-static-vector vector))))
