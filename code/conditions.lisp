(in-package #:cl-starpu)

(define-condition starpu-error ()
  ((%error-code
    :initarg :error-code
    :reader starpu-error-code))
  (:report
   (lambda (condition stream)
     (format stream "Nonzero error code: ~S"
             (starpu-error-code condition)))))

(define-condition no-worker-available (starpu-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "No worker available."))))
