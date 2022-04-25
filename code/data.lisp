(in-package #:cl-starpu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric data-name (data))

(defgeneric data-coordinates (data))

(defgeneric data-unregister (data))

(defgeneric data-unregister-no-coherency (data))

(defgeneric data-fetch-on-node (data node &key async))

(defgeneric data-prefetch-on-node (data node &key async))

(defgeneric data-invalidate (data))

(defgeneric data-invalidate-submit (data))

(defgeneric data-acquire (data &key mode node callback callback-arg sequential-consistency))

(defgeneric data-acquire-try (data &key mode node))

(defgeneric data-release (data &key node))

(defgeneric data-on-node-p (data node))

(defgeneric data-wont-use (data))

(defgeneric data-sequential-consistency (data))

(defgeneric data-default-sequential-consistency (data))

(defgeneric data-wt-mask (data))

(defgeneric data-ooc (data))

(defgeneric data-user-data (data))

(defgeneric (setf data-name) (value data))

(defgeneric (setf data-coordinates) (value data))

(defgeneric (setf data-default-sequential-consistency) (value data))

(defgeneric (setf data-wt-mask) (value data))

(defgeneric (setf data-ooc) (value data))

(defgeneric (setf data-user-data) (value data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass data ()
  ((name
    :type (or symbol string)
    :accessor data-name)
   (handle
    :type cffi:foreign-pointer
    :accessor data-handle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods
