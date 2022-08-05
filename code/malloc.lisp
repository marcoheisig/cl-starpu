(in-package :cl-starpu)

;;; StarPU provides a mechanism to override its versions of malloc and
;;; free.  We (ab)use this mechanism to create a 16 byte section before
;;; each allocation.  This section can then be used to write down SBCL's
;;; array header information, so that each StarPU allocation can also be
;;; treated as a Lisp array.
(let ((library
        (load-foreign-code
         "
#include <starpu.h>
#include <cuda.h>

extern \"C\"{
int cl_starpu_malloc(unsigned node, void** result, size_t size, int flags) {
    size += 16;
    void* ptr;
#ifdef STARPU_USE_CUDA
    if (flags & STARPU_MALLOC_PINNED) {
        cudaError_t err;
        err = cudaHostAlloc(&ptr, size, cudaHostAllocPortable);
        if (STARPU_UNLIKELY(err)) return -ENOMEM;
        goto end;
    }
#endif
    ptr = malloc(size);
    if (!ptr) return -ENOMEM;
end:
    (*result) = (void*)(((char*)ptr) + 16);
    return 0;
}

int cl_starpu_free(unsigned node, void* ptr, size_t size, int flags) {
    ptr = (void*)(((char*)ptr) - 16);
#ifdef STARPU_USE_CUDA
    if (flags & STARPU_MALLOC_PINNED) {
        cudaError_t err = cudaFreeHost(ptr);
        return 0;
    }
#endif
    free(ptr);
    return 0;
}
}
"
         :compiler (if (zerop (%starpu-cuda-worker-get-count)) "cc" "nvcc")
         :flags (list* "-O3" (pkg-config "starpu-1.3" "--cflags" "--libs")))))
  (%starpu-malloc-set-hooks
   (cffi:foreign-symbol-pointer "cl_starpu_malloc" :library library)
   (cffi:foreign-symbol-pointer "cl_starpu_free" :library library)))

(defun malloc (n)
  (cffi:with-foreign-object (pointer :pointer)
    (unless (zerop (%starpu-malloc pointer n))
      (error "Call to (malloc ~D) failed." n))
    (cffi:mem-ref pointer :pointer)))

(defun free (pointer)
  (%starpu-free pointer))

(defun static-vector-from-starpu-pointer (pointer element-type length)
  ;; Our custom version of starpu_malloc conveniently allocates 16 bytes
  ;; before each chunk of data (malloc-hack.lisp).
  (setf (sb-sys:sap-ref-word pointer -16)
        (static-vectors::vector-widetag-and-n-bits element-type))
  (setf (sb-sys:sap-ref-word pointer -8)
        (sb-vm:fixnumize length))
  (sb-kernel:%make-lisp-obj
   (logior (- (sb-sys:sap-int pointer) 16)
           sb-vm:other-pointer-lowtag)))

(defun make-pinned-array
    (dimensions
     &key
       (element-type t)
       (initial-element nil initial-element-p))
  (let* ((size (if (integerp dimensions) dimensions (reduce #'* dimensions)))
         (foreign-type (lisp-type-foreign-type element-type))
         (element-size (cffi:foreign-type-size foreign-type))
         (nbytes (* element-size size)))
    (let ((pointer (malloc nbytes)))
      ;; Initialize memory.
      (when initial-element-p
        (unless (typep initial-element element-type)
          (error "The initial element ~S is not of the specified element type ~S."
                 initial-element
                 element-type))
        (fill-foreign-memory pointer foreign-type size initial-element))
      (let ((vector (static-vector-from-starpu-pointer pointer element-type size)))
        (trivial-garbage:finalize
         (make-array dimensions :element-type element-type :displaced-to vector)
         (lambda () (free pointer)))))))

(defun pinned-array-data-pointer (array)
  (static-vectors:static-vector-pointer
   (sb-vm::%array-data array)))

#+sbcl
(let ((cache (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
  (loop for saetp across sb-vm:*specialized-array-element-type-properties* do
    (let* ((array (make-array 0 :element-type (sb-vm:saetp-specifier saetp)))
           (index (- (sb-kernel:array-underlying-widetag array) 128)))
      (setf (aref cache index)
            (ceiling (sb-vm:saetp-n-bits saetp) sb-vm:n-byte-bits))))
  (defun array-element-size (array)
    (declare (array array))
    (aref cache (- (sb-kernel:array-underlying-widetag array) 128))))

#-sbcl
(defun array-element-size (array)
  (cffi:foreign-type-size
   (foreign-type-lisp-type
    (array-element-type array))))
