(in-package #:cl-starpu)

(defparameter *foreign-type-lisp-type*
  (loop for (foreign-type lisp-type)
          in '((:char   base-char)
               (:uint8  (unsigned-byte 8))
               (:uint16 (unsigned-byte 16))
               (:uint32 (unsigned-byte 32))
               (:uint64 (unsigned-byte 64))
               (:int8   (signed-byte 8))
               (:int16  (signed-byte 16))
               (:int32  (signed-byte 32))
               (:int64  (signed-byte 64))
               (:float  single-float)
               (:double double-float))
        unless (eql (upgraded-array-element-type lisp-type) 't)
          collect (list foreign-type lisp-type)))

(defun lisp-type-foreign-type (lisp-type)
  (loop for (foreign-type other-lisp-type) in *foreign-type-lisp-type* do
    (when (equal lisp-type other-lisp-type)
      (return foreign-type))
        finally
           (error "Not a Lisp type with a corresponding foreign type: ~S" lisp-type)))

(defun make-starpu-sym (&rest things)
  (intern
   (apply #'concatenate 'string (mapcar #'string things))
   (find-package "CL-STARPU")))

(defun string-from-symbol (symbol)
  (let ((*package* (find-package "KEYWORD"))
        (*print-readably*)
        (*print-case* :upcase))
    (with-output-to-string (stream)
      (format stream "~S" symbol))))
