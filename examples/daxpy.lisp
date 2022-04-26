(in-package #:cl-starpu-examples)

;;; This example illustrates how StarPU can be used to compute the in-place
;;; vector-scalar product y[i] := a * x[i] + y[i] using double-precision
;;; floating-point numbers.

(cffi:defcallback daxpy :void ((buffers :pointer) (cl-arg :pointer))
  (multiple-value-bind (a) (starpu:unpack-arguments cl-arg :double)
    (let* ((x (cffi:mem-ref buffers :pointer 0))
           (y (cffi:mem-ref buffers :pointer 1))
           (x-nx (starpu::%starpu-vector-get-nx x))
           (y-nx (starpu::%starpu-vector-get-nx y))
           (nx (if (= x-nx y-nx)
                   x-nx
                   (error "Both vectors supplied to DAXPY must have the same size.")))
           (x-ptr (starpu::%starpu-vector-get-local-ptr x))
           (y-ptr (starpu::%starpu-vector-get-local-ptr y)))
      (dotimes (i nx)
        (setf (cffi:mem-ref y-ptr :double i)
              (+ (* a (cffi:mem-ref x-ptr :double i))
                 (cffi:mem-ref y-ptr :double i)))))))

(defun daxpy (a x y)
  (declare (double-float a)
           (starpu:vector-handle x y))
  (starpu:initialize)
  (print (list :before (starpu::data-handle-handle x) (starpu::data-handle-handle y) (starpu::%starpu-vector-get-nx (starpu::data-handle-handle x))))
  (starpu:task-insert
   (make-instance 'starpu:codelet
     :name "daxpy"
     :cpu-func-0 (cffi:callback daxpy)
     :modes '(:r :rw))
   :double a :r x :rw y))

(defun run-daxpy (&key (length 100) (a 2d0))
  (let ((x (starpu:make-vector-handle length :element-type 'double-float :initial-element 2d0))
        (y (starpu:make-vector-handle length :element-type 'double-float :initial-element 1d0)))
    (daxpy a x y)
    (starpu:task-wait-for-all)
    (format t "~&Y[0] = ~S" (aref (starpu:vector-handle-vector y) 0))
    y))
