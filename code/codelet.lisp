(in-package #:cl-starpu)

(defstruct (codelet
            (:constructor nil)
            (:predicate codeletp))
  (handle (alexandria:required-argument :handle)
   :type cffi:foreign-pointer
   :read-only t))

(defstruct (sequential-codelet
            (:include codelet)
            (:constructor %make-sequential-codelet (handle))
            (:predicate sequential-codelet-p)))

(defstruct (spmd-codelet
            (:include codelet)
            (:constructor %make-spmd-codelet (handle))
            (:predicate spmd-codelet-p)))

(defstruct (forkjoin-codelet
            (:include codelet)
            (:constructor %make-forkjoin-codelet (handle))
            (:predicate forkjoin-codelet-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Codelet Handle Access Macros

(defmacro codelet-handle-slot-ref (handle slot-name)
  `(cffi:foreign-slot-value ,handle '(:struct %starpu-codelet) ,slot-name))

(defmacro codelet-handle-slot-pointer (handle slot-name)
  `(cffi:foreign-slot-pointer ,handle '(:struct %starpu-codelet) ,slot-name))

(defmacro codelet-handle-slot-aref (handle slot-name slot-type index)
  `(cffi:mem-aref
    (cffi:foreign-slot-pointer ,handle '(:struct %starpu-codelet) ,slot-name)
    ,slot-type
    ,index))

(defmacro codelet-handle-name (handle)
  `(codelet-handle-slot-ref ,handle '%name))

(defmacro codelet-handle-type (handle)
  `(codelet-handle-slot-ref ,handle '%type))

(defmacro codelet-handle-max-parallelism (handle)
  `(codelet-handle-slot-ref ,handle '%max-parallelism))

(defmacro codelet-handle-where (handle)
  `(codelet-handle-slot-ref ,handle '%where))

(defmacro codelet-handle-can-execute (handle)
  `(codelet-handle-slot-ref ,handle '%can-execute))

(defmacro codelet-handle-performance-model (handle)
  `(codelet-handle-slot-ref ,handle '%model))

(defmacro codelet-handle-energy-model (handle)
  `(codelet-handle-slot-ref ,handle '%energy-model))

(defmacro codelet-handle-cpu-func (handle index)
  `(codelet-handle-slot-aref ,handle '%cpu-funcs :pointer ,index))

(defmacro codelet-handle-cuda-func (handle index)
  `(codelet-handle-slot-aref ,handle '%cuda-funcs :pointer ,index))

(defmacro codelet-handle-opencl-func (handle index)
  `(codelet-handle-slot-aref ,handle '%opencl-funcs :pointer ,index))

(defmacro codelet-handle-nbuffers (handle)
  `(codelet-handle-slot-ref ,handle '%nbuffers))

(defmacro codelet-handle-modes (handle)
  `(codelet-handle-slot-pointer ,handle '%modes))

(defmacro codelet-handle-dyn-modes (handle)
  `(codelet-handle-slot-ref ,handle '%dyn-modes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Codelet Access Functions

(defun codelet-name (codelet)
  (let* ((handle (codelet-handle codelet))
         (char* (codelet-handle-name handle)))
    (if (cffi:null-pointer-p char*)
        nil
        (read-from-string
         (cffi:foreign-string-to-lisp char*)))))

(defun codelet-nbuffers (codelet)
  (codelet-handle-nbuffers (codelet-handle codelet)))

(defun codelet-max-parallelism (codelet)
  (codelet-handle-max-parallelism (codelet-handle codelet)))

(defun codelet-can-execute (codelet)
  (codelet-handle-can-execute (codelet-handle codelet)))

(defun codelet-cpu-func (codelet index)
  (check-type index (integer 0 (#.+starpu-maximplementations+)))
  (codelet-handle-cpu-func (codelet-handle codelet) index))

(defun (setf codelet-cpu-func) (value codelet index)
  (check-type index (integer 0 (#.+starpu-maximplementations+)))
  (setf (codelet-handle-cpu-func (codelet-handle codelet) index)
        value))

(defun codelet-cuda-func (codelet index)
  (check-type index (integer 0 (#.+starpu-maximplementations+)))
  (codelet-handle-cuda-func (codelet-handle codelet) index))

(defun (setf codelet-cuda-func) (value codelet index)
  (check-type index (integer 0 (#.+starpu-maximplementations+)))
  (setf (codelet-handle-cuda-func (codelet-handle codelet) index)
        value))

(defun codelet-opencl-func (codelet index)
  (check-type index (integer 0 (#.+starpu-maximplementations+)))
  (codelet-handle-opencl-func (codelet-handle codelet) index))

(defmethod (setf codelet-opencl-func) (value codelet index)
  (check-type index (integer 0 (#.+starpu-maximplementations+)))
  (setf (codelet-handle-opencl-func (codelet-handle codelet) index)
        value))

(defun codelet-performance-model (codelet)
  (codelet-handle-performance-model (codelet-handle codelet)))

(defun (setf codelet-performance-model) (value codelet)
  (setf (codelet-handle-performance-model (codelet-handle codelet))
        value))

(defun codelet-energy-model (codelet)
  (codelet-handle-energy-model (codelet-handle codelet)))

(defmethod (setf codelet-energy-model) (value codelet)
  (setf (codelet-handle-energy-model (codelet-handle codelet))
        value))

(defun codelet-mode (codelet index)
  (let ((handle (codelet-handle codelet)))
    (assert (< index (codelet-handle-nbuffers handle)))
    (cffi:mem-aref
     (if (cffi:null-pointer-p (codelet-handle-dyn-modes handle))
         (codelet-handle-slot-pointer handle '%modes)
         (codelet-handle-slot-ref handle '%dyn-modes))
     'starpu-data-access-mode
     index)))

(defun codelet-modes (codelet)
  (loop for index below (codelet-nbuffers codelet)
        collect (codelet-mode codelet index)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Codelet Creation

(defun make-codelet
    (&key
       (name nil)
       (max-parallelism nil max-parallelism-supplied-p)
       (modes '())
       (type :seq)
       . #.
       (loop for index below +starpu-maximplementations+
             collect (intern (format nil "CPU-FUNC-~D" index) *package*)
             collect (intern (format nil "CUDA-FUNC-~D" index) *package*)
             collect (intern (format nil "OPENCL-FUNC-~D" index) *package*)))
  (check-type name symbol)
  (let ((handle (cffi:foreign-alloc '(:struct %starpu-codelet)))
        (nbuffers (length modes)))
    (%starpu-codelet-init handle)
    (setf (codelet-handle-name handle)
          (if (null name)
              (cffi:null-pointer)
              (cffi:foreign-string-alloc (string-from-symbol name))))
    (setf (codelet-handle-type handle) type)
    (setf (codelet-handle-nbuffers handle) nbuffers)
    (when max-parallelism-supplied-p
      (setf (codelet-handle-max-parallelism handle) max-parallelism))
    (let ((modes* (if (<= nbuffers +starpu-nmaxbufs+)
                      (codelet-handle-modes handle)
                      (setf (codelet-handle-dyn-modes handle)
                            (cffi:foreign-alloc 'starpu-data-access-mode :count nbuffers)))))
      (loop for mode in modes
            for index below nbuffers do
              (setf (cffi:mem-aref modes* 'starpu-data-access-mode index)
                    mode)))
    (progn
      . #.
      (loop for index below +starpu-maximplementations+
            collect
            (let ((arg (intern (format nil "CPU-FUNC-~D" index) *package*)))
              `(unless (null ,arg)
                 (setf (codelet-handle-cpu-func handle ,index) ,arg)))
            collect
            (let ((arg (intern (format nil "CUDA-FUNC-~D" index) *package*)))
              `(unless (null ,arg)
                 (setf (codelet-handle-cuda-func handle ,index) ,arg)))
            collect
            (let ((arg (intern (format nil "OPENCL-FUNC-~D" index) *package*)))
              `(unless (null ,arg)
                 (setf (codelet-handle-opencl-func handle ,index) ,arg)))))
    (trivial-garbage:finalize
     (funcall
      (ecase type
        (:seq #'%make-sequential-codelet)
        (:spmd #'%make-spmd-codelet)
        (:forkjoin #'%make-forkjoin-codelet))
      handle)
     (lambda ()
       (cffi:foreign-string-free
        (cffi:foreign-slot-value handle '(:struct %starpu-codelet) '%name))
       (cffi:foreign-free (codelet-handle-dyn-modes handle))
       (cffi:foreign-free handle)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Miscellaneous

(defmethod print-object ((codelet codelet) stream)
  (print-unreadable-object (codelet stream :type t)
    (format stream "~@<~@{~S ~S~^ ~_~}~:>"
            :name (codelet-name codelet)
            :modes (codelet-modes codelet)
            :max-parallelism (codelet-max-parallelism codelet))))
