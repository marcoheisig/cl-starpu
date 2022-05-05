(in-package #:cl-starpu)

(defstruct (interface (:constructor nil)
                      (:predicate interfacep))
  (handle (alexandria:required-argument :handle)
   :type cffi:foreign-pointer
   :read-only t))

(defmacro with-unpacked-arguments (cl-arg foreign-objects &body body)
  "Unpack the CL-ARGS argument to a codelet function according to the
provided (NAME FOREIGN-TYPE) specification, and run BODY."
  `(multiple-value-bind ,(mapcar #'first foreign-objects)
       (cffi:with-foreign-objects ,foreign-objects
         (%starpu-codelet-unpack-args
          ,cl-arg
          ,@(loop for (name nil) in foreign-objects
                  collect :pointer
                  collect name))
         (values
          ,@(loop for (name type) in foreign-objects
                  collect `(cffi:mem-ref ,name ,type))))
     ,@body))
