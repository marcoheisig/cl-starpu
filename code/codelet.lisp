(in-package #:cl-starpu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric codeletp (object)
  (:method ((object t)) nil))

(defgeneric codelet-name (codelet))

(defgeneric codelet-type (codelet))

(defgeneric codelet-max-parallelism (codelet))

(defgeneric codelet-can-execute (codelet))

(defgeneric codelet-cpu-func (codelet index))

(defgeneric codelet-cuda-func (codelet index))

(defgeneric codelet-opencl-func (codelet index))

(defgeneric codelet-performance-model (codelet))

(defgeneric codelet-energy-model (codelet))

(defgeneric codelet-number-of-buffers (codelet))

(defgeneric (setf codelet-name) (value codelet))

(defgeneric (setf codelet-max-parallelism) (value codelet))

(defgeneric (setf codelet-can-execute) (value codelet))

(defgeneric (setf codelet-cpu-func) (value codelet index))

(defgeneric (setf codelet-cuda-func) (value codelet index))

(defgeneric (setf codelet-opencl-func) (value codelet index))

(defgeneric (setf codelet-performance-model) (value codelet))

(defgeneric (setf codelet-energy-model) (value codelet))

(defgeneric (setf codelet-number-of-buffers) (value codelet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass codelet ()
  ((name
    :type (or symbol string)
    :accessor codelet-name)
   (handle
    :type cffi:foreign-pointer
    :accessor codelet-handle)
   (number-of-buffers
    :type unsigned-byte
    :accessor codelet-number-of-buffers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Slot Access Macros

(defmacro codelet-handle-slot-ref (handle slot-name)
  `(cffi:foreign-slot-value ,handle '(:struct starpu-codelet-cstruct) ,slot-name))

(defmacro codelet-handle-slot-aref (handle slot-name slot-type index)
  `(cffi:mem-ref
    (cffi:foreign-slot-pointer ,handle '(:struct starpu-codelet-cstruct) ,slot-name)
    ,slot-type
    ,index))

(defmacro codelet-handle-name (handle)
  `(codelet-handle-slot-ref ,handle 'name-slot))

(defmacro codelet-handle-type (handle)
  `(codelet-handle-slot-ref ,handle 'type-slot))

(defmacro codelet-handle-max-parallelism (handle)
  `(codelet-handle-slot-ref ,handle 'max-parallelism-slot))

(defmacro codelet-handle-where (handle)
  `(codelet-handle-slot-ref ,handle 'where-slot))

(defmacro codelet-handle-can-execute (handle)
  `(codelet-handle-slot-ref ,handle 'can-execute-slot))

(defmacro codelet-handle-performance-model (handle)
  `(codelet-handle-slot-ref ,handle 'model-slot))

(defmacro codelet-handle-energy-model (handle)
  `(codelet-handle-slot-ref ,handle 'energy-model-slot))

(defmacro codelet-handle-cpu-func (handle index)
  `(codelet-handle-slot-aref ,handle 'cpu-funcs-slot :pointer ,index))

(defmacro codelet-handle-cuda-func (handle index)
  `(codelet-handle-slot-aref ,handle 'cuda-funcs-slot :pointer ,index))

(defmacro codelet-handle-opencl-func (handle index)
  `(codelet-handle-slot-aref ,handle 'opencl-funcs-slot :pointer ,index))

(defmacro codelet-handle-nbuffers (handle)
  `(codelet-handle-slot-ref ,handle 'nbuffers-slot))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod codeletp ((codelet codelet))
  t)

(defmethod initialize-instance :before ((codelet codelet) &key &allow-other-keys)
  (let ((handle (cffi:foreign-alloc '(:struct starpu-codelet-cstruct))))
    (%starpu-codelet-init handle)
    (setf (codelet-handle codelet) handle)
    (trivial-garbage:finalize
     codelet
     (lambda ()
       (cffi:foreign-string-free
        (cffi:foreign-slot-value handle '(:struct starpu-codelet-cstruct) 'name-slot))
       (cffi:foreign-free handle)))))

(defmethod shared-initialize
    ((codelet codelet) slot-names
     &key
       (name (gensym "CODELET-"))
       (max-parallelism nil max-parallelism-supplied-p)
       (number-of-buffers 0)
       (type :seq))
  (setf (codelet-name codelet)
        name)
  (setf (codelet-type codelet)
        type)
  (when max-parallelism-supplied-p
    (setf (codelet-max-parallelism codelet)
          max-parallelism))
  (setf (codelet-number-of-buffers codelet)
        number-of-buffers)
  (call-next-method))

(defmethod print-object ((codelet codelet) stream)
  (print-unreadable-object (codelet stream :type t)
    (format stream "~@<~@{~S ~S~^ ~_~}~:>"
            :name (codelet-name codelet)
            :type (codelet-type codelet)
            :number-of-buffers (codelet-number-of-buffers codelet))))

(defmethod codelet-type (codelet)
  (codelet-handle-type (codelet-handle codelet)))

(defmethod codelet-max-parallelism (codelet)
  (codelet-handle-max-parallelism (codelet-handle codelet)))

(defmethod codelet-can-execute (codelet)
  (codelet-handle-can-execute (codelet-handle codelet)))

(defmethod codelet-cpu-func (codelet index)
  (check-type index (integer 0 (#.+starpu-maximplementations+)))
  (codelet-handle-cpu-func (codelet-handle codelet) index))

(defmethod codelet-cuda-func (codelet index)
  (check-type index (integer 0 (#.+starpu-maximplementations+)))
  (codelet-handle-cuda-func (codelet-handle codelet) index))

(defmethod codelet-opencl-func (codelet index)
  (check-type index (integer 0 (#.+starpu-maximplementations+)))
  (codelet-handle-opencl-func (codelet-handle codelet) index))

(defmethod codelet-performance-model (codelet)
  (codelet-handle-performance-model (codelet-handle codelet)))

(defmethod codelet-energy-model (codelet)
  (codelet-handle-energy-model (codelet-handle codelet)))

(defmethod (setf codelet-name) :after (value codelet)
  (symbol-macrolet ((ptr (codelet-handle-name (codelet-handle codelet))))
    (cffi:foreign-string-free ptr))
  (setf ptr (cffi:foreign-string-alloc (string value))))

(defmethod (setf codelet-type) (value codelet)
  (setf (codelet-handle-type (codelet-handle codelet))
        value))

(defmethod (setf codelet-cpu-func) (value codelet index)
  (check-type index (integer 0 (#.+starpu-maximplementations+)))
  (setf (codelet-handle-cpu-func (codelet-handle codelet) index)
        value))

(defmethod (setf codelet-cuda-func) (value codelet index)
  (check-type index (integer 0 (#.+starpu-maximplementations+)))
  (setf (codelet-handle-cuda-func (codelet-handle codelet) index)
        value))

(defmethod (setf codelet-opencl-func) (value codelet index)
  (check-type index (integer 0 (#.+starpu-maximplementations+)))
  (setf (codelet-handle-opencl-func (codelet-handle codelet) index)
        value))

(defmethod (setf codelet-performance-model) (value codelet)
  (setf (codelet-handle-performance-model (codelet-handle codelet))
        value))

(defmethod (setf codelet-energy-model) (value codelet)
  (setf (codelet-handle-energy-model (codelet-handle codelet))
        value))

(defmethod (setf codelet-number-of-buffers) :after (value codelet)
  (setf (codelet-handle-nbuffers (codelet-handle codelet))
        (if (<= value +starpu-nmaxbufs+)
            value
            +starpu-variable-nbuffers+)))
