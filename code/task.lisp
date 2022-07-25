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

(defun task-wait (task)
  (declare (task task))
  (%starpu-task-wait (task-handle task)))

(defun task-wait-for-all ()
  (%starpu-task-wait-for-all))

(defmacro task-insert
    (codelet
     &rest plist
     &key . #1=#.
       (mapcar
        (lambda (keyword)
          (intern (symbol-name keyword) *package*))
        (append
         '(:r :w :rw :scratch :redux :data :args
           :execute-on-worker :worker-order
           :callback :callback-arg :priority :tag :tag-only
           :flops :sched-ctx :handles-sequential-consistency
           :deps-array :color :synchronous :end-dep)
          *foreign-types*)))
  (declare (ignore . #1#))
  (parse-task-insert codelet plist))

(defun parse-task-insert (codelet-form args)
  (let* ((codelet (gensym "CODELET"))
         (reversed-bindings '())
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
             (push-initform (form)
               (push form reversed-initforms))
             (push-foreign-object (var type &optional (count 1))
               (push `(,var ,type ,count) reversed-foreign-objects)))
      (push-binding codelet `(codelet-handle ,codelet-form))
      (loop for rest = args then (cddr rest) until (null rest) do
        (unless (cdr rest)
          (error "Odd number of task insert keywords in ~S." args))
        (let ((key (first rest))
              (value-form (second rest))
              (value (gensym)))
          (push-binding value value-form)
          (case key
            ((:args)
             (alexandria:with-gensyms (args size)
               (push-binding args (cffi:null-pointer))
               (push-binding size 0)
               (push-initform `(multiple-value-setq (,args ,size)
                                 (apply #'pack-task-insert-args ,value)))
               (push-arg :int +starpu-cl-args+)
               (push-arg :pointer args)
               (push-arg :size size)))
            ((:r :w :rw :scratch :redux)
             (push-arg 'starpu-data-access-mode key)
             (push-arg :pointer `(data-handle ,value)))
            ((:data)
             (alexandria:with-gensyms (data length rest index)
               (push-binding length `(floor (list-length ,value) 2))
               (push-foreign-object data '(:struct %starpu-data-descr) length)
               (push-initform
                `(loop for ,index from 0 below ,length
                       for ,rest = ,value then (cddr ,value) until (null ,rest)
                       do (setf (cffi:foreign-slot-value
                              (cffi:mem-aptr ,data '(:struct %starpu-data-descr) ,index)
                              '(:struct %starpu-data-descr)
                               '%mode)
                             (first rest))
                       (setf (cffi:foreign-slot-value
                                 (cffi:mem-aptr ,data '(:struct %starpu-data-descr) ,index)
                                 '(:struct %starpu-data-descr)
                                  '%handle)
                                (data-handle (second rest)))))
               (push-arg :int +starpu-data-mode-array+)
               (push-arg :pointer data)
               (push-arg :int length)))
            ((:callback)
             (push-arg :int +starpu-callback+)
             (push-arg :pointer value))
            ((:callback-arg)
             (push-arg :int +starpu-callback-arg+)
             (push-arg :pointer value))
            ((:priority)
             (push-arg :int +starpu-priority+)
             (push-arg :int value))
            ((:tag)
             (push-arg :int +starpu-tag+)
             (push-arg :uint64 value))
            ((:flops)
             (push-arg :int +starpu-flops+)
             (push-arg :double value))
            ((:execute-on-worker)
             (push-arg :int +starpu-execute-on-worker+)
             (push-arg :int `(worker-id ,value)))
            ((:synchronous)
             (push-arg :int +starpu-task-synchronous+)
             (push-arg :int `(if ,value 1 0)))
            (#.*foreign-types*
             (push-arg :int +starpu-value+)
             (let ((object (gensym)))
               (push-foreign-object object key)
               (push-initform `(setf (cffi:mem-ref ,object ,key) ,value))
               (push-arg :pointer object)
               (push-arg :size (cffi:foreign-type-size key))))
            (otherwise
             (error "Unknown task insert key: ~S" key)))))
      `(let* ,(bindings)
         (cffi:with-foreign-objects ,(foreign-objects)
           ,@(initforms)
           (%starpu-task-insert ,codelet ,@(args) :int 0))))))

(defun pack-task-insert-args
    (&rest plist
     &key . #1=#.
       (mapcar
        (lambda (type)
          (intern (symbol-name type) *package*))
        *foreign-types*))
  (declare (ignore . #1#))
  (cffi:with-foreign-objects ((state '(:struct %starpu-codelet-pack-arg-data))
                              (args :pointer)
                              (size :size)
                              (tmp :uint8 #.(reduce #'max *foreign-types* :key #'cffi:foreign-type-size)))
    (%starpu-codelet-pack-arg-init state)
    (loop for rest = plist then (cddr rest) until (null rest) do
      (unless (cdr rest)
        (error "Odd number of keywords in ~S." plist))
      (let* ((type (first rest))
             (value (second rest))
             (size (cffi:foreign-type-size type)))
        (setf (cffi:mem-ref tmp type) value)
        (%starpu-codelet-pack-arg state tmp size)))
    (%starpu-codelet-pack-arg-fini state args size)
    (values (cffi:mem-ref args :pointer)
            (cffi:mem-ref size :size))))
