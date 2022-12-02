(in-package #:cl-starpu)

(defparameter *workers* (make-hash-table))

(defstruct (worker
            (:constructor nil)
            (:predicate workerp))
  (id 0
   :type (signed-byte 64)
   :read-only t))

(defstruct (cpu-worker
            (:include worker)
            (:constructor %make-cpu-worker (id))
            (:predicate cpu-worker-p)))

(defstruct (cuda-worker
            (:include worker)
            (:constructor %make-cuda-worker (id))
            (:predicate cuda-worker-p)))

(defstruct (opencl-worker
            (:include worker)
            (:constructor %make-opencl-worker (id))
            (:predicate opencl-worker-p)))

(defstruct (mpi-ms-worker
            (:include worker)
            (:constructor %make-mpi-ms-worker (id))
            (:predicate mpi-ms-worker-p)))

(defun %make-worker (id)
  (assert (initializedp))
  (alexandria:ensure-gethash
   id
   *workers*
   (ecase (%starpu-worker-get-type id)
     (:cpu-worker (%make-cpu-worker id))
     (:cuda-worker (%make-cuda-worker id))
     (:opencl-worker (%make-opencl-worker id))
     (:mic-worker (%make-mic-worker id))
     (:mpi-ms-worker (%make-mpi-ms-worker id)))))

(defun worker-name (worker &aux (max-chars 100))
  (declare (worker worker))
  (cffi:with-foreign-object (char* :char max-chars)
    (%starpu-worker-get-name (worker-id worker) char* max-chars)
    (cffi:foreign-string-to-lisp char* :max-chars max-chars :encoding :ascii)))

(defun worker-memory-node (worker)
  (%make-memory-node
   (%starpu-worker-get-memory-node (worker-id worker))))

(defmethod print-object ((worker worker) stream)
  (print-unreadable-object (worker stream :type t)
    (format stream "~@<~@{~S ~S~^ ~_~}~:>"
            :name (worker-name worker)
            :id (worker-id worker)
            :memory-node (worker-memory-node worker))))

(defun all-workers-with-type (type)
  (let ((n (%starpu-worker-get-count-by-type type)))
    (cffi:with-foreign-object (ids :int n)
      (let ((count (%starpu-worker-get-ids-by-type type ids n)))
        (loop for index below count
              collect (%make-worker (cffi:mem-aref ids :int index)))))))

(defun all-workers ()
  (all-workers-with-type :any-worker))

(defun all-cpu-workers ()
  (all-workers-with-type :cpu-worker))

(defun all-cuda-workers ()
  (all-workers-with-type :cuda-worker))

(defun all-opencl-workers ()
  (all-workers-with-type :opencl-worker))

(defun all-mpi-ms-workers ()
  (all-workers-with-type :mpi-ms-worker))
