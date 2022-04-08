(defsystem "cl-starpu-test-suite"
  :description "The cl-starpu Test Suite."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :version "1.0"
  :license "MIT"
  :depends-on ("cl-starpu")
  :serial t
  :components
  ((:file "packages")))
