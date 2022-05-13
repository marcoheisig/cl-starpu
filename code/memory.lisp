(in-package #:cl-starpu)

(defun starpu-malloc (n)
  (cffi:with-foreign-object (pointer :pointer)
    (unless (zerop (%starpu-malloc pointer n))
      (error "Call to (starpu-malloc ~D) failed." n))
    (cffi:mem-ref pointer :pointer)))

(defun starpu-free (pointer)
  (%starpu-free pointer))

(defun starpu-allocate
    (dimensions
     &key
       (element-type t)
       (initial-element nil initial-element-p))
  (let* ((size (if (integerp dimensions) dimensions (reduce #'* dimensions)))
         (foreign-type (lisp-type-foreign-type element-type))
         (element-size (cffi:foreign-type-size foreign-type))
         (nbytes (* element-size size)))
    (let ((pointer (starpu-malloc nbytes)))
      (when initial-element-p
        (unless (typep initial-element element-type)
          (error "The initial element ~S is not of the specified element type ~S."
                 initial-element
                 element-type))
        (fill-foreign-memory pointer foreign-type size initial-element))
      (values pointer foreign-type size))))
