(in-package #:cl-starpu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric taskp (object)
  (:method ((object t)) nil))

(defgeneric task-handle (task))

(defgeneric task-name (task))

(defgeneric task-codelet (task))

(defgeneric task-codelet-arguments (task))

(defgeneric task-number-of-buffers (task))

(defgeneric task-handles (task))

(defgeneric (setf task-name) (value task))

(defgeneric (setf task-codelet) (value task))

(defgeneric (setf task-codelet-arguments) (task))

(defgeneric (setf task-number-of-buffers) (value task))

(defgeneric (setf task-handles) (value task))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass task ()
  ((name
    :type (or symbol string)
    :accessor task-name)
   (handle
    :type cffi:foreign-pointer
    :accessor task-handle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Slot Access Macros

(defmacro task-handle-slot-ref (handle slot-name)
  `(cffi:foreign-slot-value ,handle '(:struct starpu-task-cstruct) ,slot-name))

(defmacro task-handle-name (handle)
  `(task-handle-slot-ref ,handle 'name-slot))

(defmacro task-handle-cl (handle)
  `(task-handle-slot-ref ,handle 'cl-slot))

(defmacro task-handle-where (handle)
  `(task-handle-slot-ref ,handle 'where-slot))

(defmacro task-handle-nbuffers (handle)
  `(task-handle-slot-ref ,handle 'nbuffers-slot))

(defmacro task-handle-dyn-handles (handle)
  `(task-handle-slot-ref ,handle 'dyn-handles-slot))

(defmacro task-handle-dyn-interfaces (handle)
  `(task-handle-slot-ref ,handle 'dyn-interfaces-slot))

(defmacro task-handle-dyn-modes (handle)
  `(task-handle-slot-ref ,handle 'dyn-modes-slot))

(defmacro task-handle-handles (handle)
  `(task-handle-slot-ref ,handle 'handles-slot))

(defmacro task-handle-interfaces (handle)
  `(task-handle-slot-ref ,handle 'interfaces-slot))

(defmacro task-handle-modes (handle)
  `(task-handle-slot-ref ,handle 'modes-slot))

(defmacro task-handle-cl-arg (handle)
  `(task-handle-slot-ref ,handle 'cl-arg-slot))

(defmacro task-handle-cl-arg-size (handle)
  `(task-handle-slot-ref ,handle 'cl-arg-size-slot))

(defmacro task-handle-callback-func (handle)
  `(task-handle-slot-ref ,handle 'callback-func-slot))

(defmacro task-handle-callback-arg (handle)
  `(task-handle-slot-ref ,handle 'callback-arg-slot))

(defmacro task-handle-status (handle)
  `(task-handle-slot-ref ,handle 'status-slot))

(defmacro task-handle-type (handle)
  `(task-handle-slot-ref ,handle 'type-slot))

(defmacro task-handle-color (handle)
  `(task-handle-slot-ref ,handle 'color-slot))

(defmacro task-handle-flops (handle)
  `(task-handle-slot-ref ,handle 'flops-slot))

(defmacro task-handle-predicted (handle)
  `(task-handle-slot-ref ,handle 'predicted-slot))

(defmacro task-handle-predicted-transfer (handle)
  `(task-handle-slot-ref ,handle 'predicted-transfer-slot))

(defmacro task-handle-predicted-start (handle)
  `(task-handle-slot-ref ,handle 'predicted-start-slot))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod taskp ((task task))
  t)

(defmethod initialize-instance :before ((task task) &key &allow-other-keys)
  (let ((handle (cffi:foreign-alloc '(:struct starpu-task-cstruct))))
    (%starpu-task-init handle)
    (setf (task-handle task) handle)
    (trivial-garbage:finalize
     task
     (lambda ()
       (cffi:foreign-string-free
        (cffi:foreign-slot-value handle '(:struct starpu-task-cstruct) 'name-slot))
       (cffi:foreign-free handle)))))

(defmethod shared-initialize
    ((task task) slot-names
     &key (name (gensym "TASK-")))
  (setf (task-name task)
        name)
  (call-next-method))

(defmethod print-object ((task task) stream)
  (print-unreadable-object (task stream :type t)
    (format stream "~@<~@{~S ~S~^ ~_~}~:>"
            :name (task-name task))))

(defmethod (setf task-name) :after (value (task task))
  (symbol-macrolet ((ptr (task-handle-name (task-handle task))))
    (cffi:foreign-string-free ptr))
  (setf ptr (cffi:foreign-string-alloc (string value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions

(defmacro starpu-task-insert
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
           :task-deps-array :task-color :task-synchronous :task-end-dep)
          cffi:*built-in-foreign-types*)))
  (declare (ignore . #1#))
  (parse-starpu-task-insert-form codelet args))

(defun parse-starpu-task-insert-form (codelet args)
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
             (push-arg :pointer value-sym))
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
            ((:modes :execute-on-worker :worker-order :tag-only :sched-ctx
                     :handles-sequential-consistency :task-deps-array :task-color
                     :task-synchronous :task-end-dep)
             (break "TODO"))
            (otherwise
             (error "Unknown task insert key: ~S" key)))))
      `(let ,(bindings)
         (cffi:with-foreign-objects ,(foreign-objects)
           ,@(initforms)
           (%starpu-task-insert (codelet-handle ,codelet) ,@(args) :int 0))))))

(defmacro unpack-arguments (cl-arg &rest foreign-types)
  (let ((foreign-objects
          (loop for foreign-type in foreign-types
                collect `(,(gensym) ,foreign-type))))
    `(cffi:with-foreign-objects ,foreign-objects
       (%starpu-codelet-unpack-args
        ,cl-arg
        ,@(loop for (name nil) in foreign-objects
                collect :pointer
                collect name))
       (values
        ,@(loop for (name type) in foreign-objects
                collect `(cffi:mem-ref ,name ,type))))))
