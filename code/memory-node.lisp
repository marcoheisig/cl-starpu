(in-package #:cl-starpu)

(defparameter *memory-nodes* (make-hash-table))

(defstruct (memory-node (:constructor nil)
                        (:predicate memory-node-p))
  (id 0
   :type (signed-byte 64)
   :read-only t))

(defstruct (unused-memory-node
            (:include memory-node)
            (:constructor %make-unused-memory-node (id))
            (:predicate unused-memory-node-p)))

(defstruct (cpu-memory-node
            (:include memory-node)
            (:constructor %make-cpu-memory-node (id))
            (:predicate cpu-memory-node-p)))

(defstruct (cuda-memory-node
            (:include memory-node)
            (:constructor %make-cuda-memory-node (id))
            (:predicate cuda-memory-node-p)))

(defstruct (opencl-memory-node
            (:include memory-node)
            (:constructor %make-opencl-memory-node (id))
            (:predicate opencl-memory-node-p)))

(defstruct (disk-memory-node
            (:include memory-node)
            (:constructor %make-disk-memory-node (id))
            (:predicate disk-memory-node-p)))

(defstruct (mic-memory-node
            (:include memory-node)
            (:constructor %make-mic-memory-node (id))
            (:predicate mic-memory-node-p)))

(defstruct (mpi-ms-memory-node
            (:include memory-node)
            (:constructor %make-mpi-ms-memory-node (id))
            (:predicate mpi-ms-memory-node-p)))

(defun %make-memory-node (id)
  (assert (initializedp))
  (alexandria:ensure-gethash
   id
   *memory-nodes*
   (ecase (%starpu-node-get-kind id)
     (:unused (%make-unused-memory-node id))
     (:cpu-ram (%make-cpu-memory-node id))
     (:cuda-ram (%make-cuda-memory-node id))
     (:opencl-ram (%make-opencl-memory-node id))
     (:disk-ram (%make-disk-memory-node id))
     (:mic-ram (%make-mic-memory-node id))
     (:mpi-ms-ram (%make-mpi-ms-memory-node id)))))

(defun memory-node-name (memory-node &aux (max-chars 100))
  (declare (memory-node memory-node))
  (cffi:with-foreign-object (char* :char max-chars)
    (%starpu-memory-node-get-name (memory-node-id memory-node) char* max-chars)
    (cffi:foreign-string-to-lisp char* :max-chars max-chars :encoding :ascii)))

(defun memory-node-available-memory (memory-node)
  (declare (memory-node memory-node))
  (%starpu-memory-get-available (memory-node-id memory-node)))

(defun memory-node-total-memory (memory-node)
  (declare (memory-node memory-node))
  (%starpu-memory-get-total (memory-node-id memory-node)))

(defun memory-node-numa-id (memory-node)
  (declare (memory-node memory-node))
  (%starpu-memory-nodes-numa-id-to-devid (memory-node-id memory-node)))

(defmethod print-object ((memory-node memory-node) stream)
  (print-unreadable-object (memory-node stream :type t)
    (format stream "~@<~@{~S ~S~^ ~_~}~:>"
            :name (memory-node-name memory-node)
            :id (memory-node-id memory-node)
            :available-memory (memory-node-available-memory memory-node)
            :total-memory (memory-node-total-memory memory-node)
            :numa-id (memory-node-numa-id memory-node))))

;;; This variable is set during starpu:initialize
(defvar *main-memory-node* nil)
