(in-package #:cl-starpu)

(defstruct (task
            (:constructor nil)
            (:predicate taskp))
  (handle (alexandria:required-argument :handle)
   :type cffi:foreign-pointer
   :read-only t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Task Handle Access Macros

(defmacro task-handle-slot-ref (handle slot-name)
  `(cffi:foreign-slot-value ,handle '(:struct %starpu-task) ,slot-name))

(defmacro task-handle-name (handle)
  `(task-handle-slot-ref ,handle '%name))

(defmacro task-handle-cl (handle)
  `(task-handle-slot-ref ,handle '%cl))

(defmacro task-handle-where (handle)
  `(task-handle-slot-ref ,handle '%where))

(defmacro task-handle-nbuffers (handle)
  `(task-handle-slot-ref ,handle '%nbuffers))

(defmacro task-handle-dyn-handles (handle)
  `(task-handle-slot-ref ,handle '%dyn-handles))

(defmacro task-handle-dyn-interfaces (handle)
  `(task-handle-slot-ref ,handle '%dyn-interfaces))

(defmacro task-handle-dyn-modes (handle)
  `(task-handle-slot-ref ,handle '%dyn-modes))

(defmacro task-handle-handles (handle)
  `(task-handle-slot-ref ,handle '%handles))

(defmacro task-handle-interfaces (handle)
  `(task-handle-slot-ref ,handle '%interfaces))

(defmacro task-handle-modes (handle)
  `(task-handle-slot-ref ,handle '%modes))

(defmacro task-handle-cl-arg (handle)
  `(task-handle-slot-ref ,handle '%cl-arg))

(defmacro task-handle-cl-arg-size (handle)
  `(task-handle-slot-ref ,handle '%cl-arg-size))

(defmacro task-handle-callback-func (handle)
  `(task-handle-slot-ref ,handle '%callback-func))

(defmacro task-handle-callback-arg (handle)
  `(task-handle-slot-ref ,handle '%callback-arg))

(defmacro task-handle-status (handle)
  `(task-handle-slot-ref ,handle '%status))

(defmacro task-handle-type (handle)
  `(task-handle-slot-ref ,handle '%type))

(defmacro task-handle-color (handle)
  `(task-handle-slot-ref ,handle '%color))

(defmacro task-handle-flops (handle)
  `(task-handle-slot-ref ,handle '%flops))

(defmacro task-handle-predicted (handle)
  `(task-handle-slot-ref ,handle '%predicted))

(defmacro task-handle-predicted-transfer (handle)
  `(task-handle-slot-ref ,handle '%predicted-transfer))

(defmacro task-handle-predicted-start (handle)
  `(task-handle-slot-ref ,handle '%predicted-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions

(defmacro task-insert
    (codelet &rest args
     &key . #1=#.
       (mapcar
        (lambda (type)
          (intern (symbol-name type) *package*))
        (append
         '(:r :w :rw :scratch :redux :data :modes
           :execute-on-worker :worker-order
           :callback :callback-arg :priority :tag :tag-only
           :flops :sched-ctx :handles-sequential-consistency
           :deps-array :color :synchronous :end-dep)
          cffi:*built-in-foreign-types*)))
  (declare (ignore . #1#))
  (parse-task-insert codelet args))

(defun parse-task-insert (codelet args)
  (let* ((reversed-bindings '())
         (reversed-args '())
         (reversed-foreign-objects '())
         (reversed-initforms '()))
    (labels ((bindings () (reverse reversed-bindings))
             (args () (reverse reversed-args))
             (foreign-objects () (reverse reversed-foreign-objects))
             (initforms () (reverse reversed-initforms))
             (push-binding (symbol form)
               (push `(,symbol ,form) reversed-bindings))
             (push-arg (type arg)
               (push type reversed-args)
               (push arg reversed-args))
             (push-foreign-object (var type value)
               (push `(,var ,type) reversed-foreign-objects)
               (push `(setf (cffi:mem-ref ,var ,type) ,value) reversed-initforms)))
      (loop for rest = args then (cddr rest) until (null rest) do
        (unless (cdr rest)
          (error "Odd number of task insert keywords in ~S." args))
        (let ((key (first rest))
              (value (second rest))
              (value-sym (gensym)))
          (push-binding value-sym value)
          (case key
            ((:r :w :rw :scratch :redux)
             (push-arg 'starpu-data-access-mode key)
             (push-arg :pointer `(data-handle ,value-sym)))
            (#.cffi:*built-in-foreign-types*
             (push-arg :int +starpu-value+)
             (let ((foreign-object (gensym)))
               (push-foreign-object foreign-object key value-sym)
               (push-arg :pointer foreign-object)
               (push-arg :size (cffi:foreign-type-size key))))
            ((:callback)
             (push-arg :int +starpu-callback+)
             (push-arg :pointer value-sym))
            ((:callback-arg)
             (push-arg :int +starpu-callback-arg+)
             (push-arg :pointer value-sym))
            ((:priority)
             (push-arg :int +starpu-priority+)
             (push-arg :int value-sym))
            ((:tag)
             (push-arg :int +starpu-tag+)
             (push-arg :uint64 value-sym))
            ((:flops)
             (push-arg :int +starpu-flops+)
             (push-arg :double value-sym))
            ((:execute-on-worker)
             (push-arg :int +starpu-execute-on-worker+)
             (push-arg :int `(worker-id ,value-sym)))
            ((:synchronous)
             (push-arg :int +starpu-task-synchronous+)
             (push-arg :int `(if ,value-sym 1 0)))
            ((:modes :worker-order :tag-only :sched-ctx
                     :handles-sequential-consistency :deps-array :color
                     :end-dep)
             (break "TODO"))
            (otherwise
             (error "Unknown task insert key: ~S" key)))))
      `(let ,(bindings)
         (cffi:with-foreign-objects ,(foreign-objects)
           ,@(initforms)
           (%starpu-task-insert (codelet-handle ,codelet) ,@(args) :int 0))))))

(defun task-wait (task)
  (declare (task task))
  (%starpu-task-wait (task-handle task)))

(defun task-wait-for-all ()
  (%starpu-task-wait-for-all))
