(in-package #:cl-starpu)

(defstruct (data (:constructor nil)
                 (:predicate datap))
  (handle (alexandria:required-argument :handle)
   :type cffi:foreign-pointer
   :read-only t)
  (contents (alexandria:required-argument :array)
   :type array
   :read-only t))

(defun data-local-pointer (data)
  (let* ((handle (data-handle data))
         (pointer (%starpu-data-get-local-ptr handle)))
    (when (cffi:null-pointer-p pointer)
      (error "Data handle currently has no local pointer."))
    ))

(defun data-total-size (data)
  (floor (%starpu-data-get-size (data-handle data))
         (array-element-size (data-contents data))))

(defun data-element-size (data)
  (array-element-size (data-contents data)))

(defun data-prefetch (data &key (memory-node *main-memory-node*) (blocking nil))
  (%starpu-data-prefetch-on-node
   (data-handle data)
   (memory-node-id memory-node)
   (if blocking 0 1)))

(defun data-fetch (data &key (memory-node *main-memory-node*) (blocking t))
  (%starpu-data-fetch-on-node
   (data-handle data)
   (memory-node-id memory-node)
   (if blocking 0 1)))

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

(defun data-partition (data filter)
  )

(defun data-unpartition (data &key (memory-node *main-memory-node*))
  )
