(in-package #:cl-starpu)

(deftype unsigned-fixnum ()
  '(and unsigned-byte fixnum))

(defparameter *foreign-type-lisp-type*
  (loop for (foreign-type lisp-type)
          in '((:char   (signed-byte 8))
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

(defun foreign-type-lisp-type (foreign-type)
  (loop for (other-foreign-type lisp-type) in *foreign-type-lisp-type* do
    (when (eq foreign-type other-foreign-type)
      (return lisp-type))
        finally
           (error "Not a foreign type with a corresponding Lisp type: ~S" foreign-type)))

(defparameter *foreign-fillers* (make-hash-table :test #'eq))

(defun foreign-filler (foreign-type)
  (alexandria:ensure-gethash
   foreign-type
   *foreign-fillers*
   (compile
    nil
    (let ((lisp-type (foreign-type-lisp-type foreign-type)))
      `(lambda (pointer size value)
         (declare (type cffi:foreign-pointer pointer)
                  (type unsigned-fixnum size)
                  (optimize (speed 3) (safety 0)))
         (check-type value ,lisp-type)
         (loop for index below size do
           (setf (cffi:mem-aref pointer ,foreign-type index)
                 value)))))))

(defun fill-foreign-memory (pointer foreign-type size value)
  (declare (cffi:foreign-pointer pointer)
           (unsigned-fixnum size))
  (funcall (foreign-filler foreign-type) pointer size value))

;; Populate the foreign fillers.
(loop for (foreign-type lisp-type) in *foreign-type-lisp-type* do
  (fill-foreign-memory (cffi:null-pointer) foreign-type 0 (coerce 0 lisp-type)))

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
