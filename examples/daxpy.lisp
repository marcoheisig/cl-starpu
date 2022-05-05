(in-package #:cl-starpu-examples)

;;; This example illustrates how StarPU can be used to compute the in-place
;;; vector-scalar product y[i] := a * x[i] + y[i] using double-precision
;;; floating-point numbers.

(cffi:defcallback daxpy :void ((buffers :pointer) (cl-arg :pointer))
  (starpu:with-unpacked-arguments cl-arg ((a :double))
    (let* ((x (cffi:mem-aref buffers :pointer 0))
           (y (cffi:mem-aref buffers :pointer 1))
           (x-ptr (cffi:foreign-slot-value x '(:struct starpu::%starpu-vector-interface) 'starpu::%ptr))
           (y-ptr (cffi:foreign-slot-value y '(:struct starpu::%starpu-vector-interface) 'starpu::%ptr))
           (x-nx (cffi:foreign-slot-value x '(:struct starpu::%starpu-vector-interface) 'starpu::%nx))
           (y-nx (cffi:foreign-slot-value y '(:struct starpu::%starpu-vector-interface) 'starpu::%nx)))
      (assert (= x-nx y-nx))
      (dotimes (i y-nx)
        (setf (cffi:mem-aref y-ptr :double i)
              (+
               (cffi:mem-aref y-ptr :double i)
               (* a (cffi:mem-aref x-ptr :double i))))))))

(defun daxpy (a x y)
  (declare (double-float a) (starpu:vector x y))
  (starpu:initialize)
  (starpu:task-insert
   (starpu:make-codelet
    :name 'daxpy
    :cpu-func-0 (cffi:callback daxpy)
    :modes '(:r :rw))
   :double a :r x :rw y))

(defun run-daxpy (&key (length 100) (a 2d0))
  (let ((x (starpu:make-vector length :element-type 'double-float :initial-element 2d0))
        (y (starpu:make-vector length :element-type 'double-float :initial-element 1d0)))
    (daxpy a x y)
    (starpu:task-wait-for-all)
    y))
