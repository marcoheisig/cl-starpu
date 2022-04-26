(defsystem "cl-starpu-examples"
  :description "A collection of cl-starpu example applications."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :version "1.0"
  :license "MIT"
  :depends-on ("cl-starpu")
  :serial t
  :components
  ((:file "packages")
   (:file "hello-world")
   (:file "codelet-arguments")
   (:file "daxpy")))
