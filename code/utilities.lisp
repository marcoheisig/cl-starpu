(in-package #:cl-starpu)

(deftype unsigned-fixnum ()
  '(and unsigned-byte fixnum))

(defparameter *foreign-type-lisp-type*
  `((:float  single-float)
    (:float  short-float)
    (:double double-float)
    (:double long-float)
    ,@(loop for type in '(:char :short :int :long :long-long
                          :int8 :int16 :int32 :int64)
            collect `(,type (signed-byte ,(cffi:foreign-type-size type))))
    ,@(loop for type in '(:unsigned-char :unsigned-short :unsigned-int
                          :unsigned-long :unsigned-long-long
                          :uint8 :uint16 :uint32 :uint64)
            collect `(,type (unsigned-byte ,(cffi:foreign-type-size type))))))

(defparameter *foreign-types*
  (remove-duplicates
   (mapcar #'first *foreign-type-lisp-type*)))

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
