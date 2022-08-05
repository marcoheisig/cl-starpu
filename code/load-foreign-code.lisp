(in-package #:cl-starpu)

(defun pkg-config (lib-name &rest options)
  (values
   (split-sequence:split-sequence
    #\space
    (uiop:run-program
     (list* "pkg-config" lib-name options)
     :output '(:string :stripped t)))))

(defun load-foreign-code
    (source-code
     &key
       (compiler "cc")
       (language "c++")
       (standard "c++14")
       (flags '()))
  (declare (string code compile language standard)
           (list flags))
  (uiop:with-temporary-file (:pathname library :type "so" :keep t)
    :close-stream
    (let* ((command
             (append
              (list compiler "-shared" "-x" language (format nil "-std=~A" standard))
              flags
              (list "-o" (uiop:native-namestring library) "-")))
           (process-info (uiop:launch-program
                          command
                          :input :stream
                          :error-output :stream))
           (input (uiop:process-info-input process-info))
           (error-output  (uiop:process-info-error-output process-info)))
      (unwind-protect (princ source-code input)
        (close input))
      (unless (zerop (uiop:wait-process process-info))
        (error "Error while compiling a shared library:~%~A~%~A"
               (with-output-to-string (stream)
                 (loop for string in command do
                   (write-string string stream)
                   (write-char #\space stream)))
               (alexandria:read-stream-content-into-string error-output))))
    (cffi:load-foreign-library library)))
