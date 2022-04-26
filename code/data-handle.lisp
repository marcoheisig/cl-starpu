(in-package #:cl-starpu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric data-handle-coordinates (data))

(defgeneric data-handle-unregister (data))

(defgeneric data-handle-unregister-no-coherency (data))

(defgeneric data-handle-fetch-on-node (data node &key async))

(defgeneric data-handle-prefetch-on-node (data node &key async))

(defgeneric data-handle-invalidate (data))

(defgeneric data-handle-invalidate-submit (data))

(defgeneric data-handle-acquire (data &key mode node callback callback-arg sequential-consistency))

(defgeneric data-handle-acquire-try (data &key mode node))

(defgeneric data-handle-release (data &key node))

(defgeneric data-handle-on-node-p (data node))

(defgeneric data-handle-wont-use (data))

(defgeneric data-handle-sequential-consistency (data))

(defgeneric data-handle-default-sequential-consistency (data))

(defgeneric data-handle-wt-mask (data))

(defgeneric data-handle-ooc (data))

(defgeneric data-handle-user-data (data))

(defgeneric (setf data-handle-coordinates) (value data))

(defgeneric (setf data-handle-default-sequential-consistency) (value data))

(defgeneric (setf data-handle-wt-mask) (value data))

(defgeneric (setf data-handle-ooc) (value data))

(defgeneric (setf data-handle-user-data) (value data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass data-handle ()
  ((handle
    :initarg :handle
    :type cffi:foreign-pointer
    :accessor data-handle-handle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

;;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Miscellaneous

(defun data-handle-p (x)
  (typep x 'data-handle))

(defparameter *foreign-type-lisp-type*
  (loop for (foreign-type lisp-type)
          in '((:char   base-char)
               (:uint8  (unsigned-byte 8))
               (:uint16 (unsigned-byte 16))
               (:uint32 (unsigned-byte 32))
               (:uint64 (unsigned-byte 64))
               (:int8   (signed-byte 8))
               (:int16  (signed-byte 16))
               (:int32  (signed-byte 32))
               (:int64  (signed-byte 64))
               (:float  single-float)
               (:double double-float))
        unless (eql (upgraded-array-element-type lisp-type) 't)
          collect (list foreign-type lisp-type)))

(defun lisp-type-foreign-type (lisp-type)
  (loop for (foreign-type other-lisp-type) in *foreign-type-lisp-type* do
    (when (equal lisp-type other-lisp-type)
      (return foreign-type))
        finally
           (error "Not a Lisp type with a corresponding foreign type: ~S" lisp-type)))
