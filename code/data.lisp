(in-package #:cl-starpu)

(defstruct (data (:constructor nil)
                 (:predicate datap))
  (handle (alexandria:required-argument :handle)
   :type cffi:foreign-pointer
   :read-only t)
  (foreign-type nil
   :read-only t)
  (element-type nil
   :read-only t))

(defun data-pointer (data)
  (%starpu-data-get-local-ptr (data-handle data)))

(defun data-size (data)
  (floor (%starpu-data-get-size (data-handle data))
         (cffi:foreign-type-size (data-foreign-type data))))

(defun data-element-size (data)
  (cffi:foreign-type-size (data-foreign-type data)))

(defun data-prefetch (data &key (memory-node *main-memory-node*) (async t))
  (%starpu-data-prefetch-on-node
   (data-handle data)
   (memory-node-id memory-node)
   (if async 1 0)))

(defun data-fetch (data &key (memory-node *main-memory-node*) (async t))
  (%starpu-data-fetch-on-node
   (data-handle data)
   (memory-node-id memory-node)
   (if async 1 0)))

(defun data-acquire
    (data &key
            (memory-node *main-memory-node*)
            (mode :r)
            (callback (cffi:null-pointer))
            (arg (cffi:null-pointer)))
  (if (cffi:null-pointer-p callback)
      (%starpu-data-acquire-on-node (data-handle data) (memory-node-id memory-node) mode)
      (%starpu-data-acquire-on-node-cb (data-handle data) (memory-node-id memory-node) mode callback arg)))

(defun data-release
    (data &key (memory-node *main-memory-node*))
  (%starpu-data-release-on-node (data-handle data) (memory-node-id memory-node)))

(defmacro with-acquired-data
    ((data &rest args &key memory-node mode callback arg sequential-consistency)
     &body body)
  (declare (ignore memory-node mode callback arg sequential-consistency))
  (alexandria:once-only (data)
    `(unwind-protect (progn (data-acquire ,data ,@args) ,@body)
       (data-release ,data))))

(defun data-contents (data)
  "Returns the contents of DATA as a specialized vector."
  (with-acquired-data (data)
    (let* ((size (data-size data))
           (pointer (data-pointer data))
           (foreign-type (data-foreign-type data))
           (element-type (foreign-type-lisp-type foreign-type))
           (vector (make-array size :element-type element-type)))
      (loop for index below size do
        (setf (aref vector index)
              (cffi:mem-aref pointer foreign-type index)))
      vector)))

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
