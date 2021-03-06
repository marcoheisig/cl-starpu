(defsystem "cl-starpu"
  :description "A Common Lisp Interface for StarPU"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :version "1.0"
  :license "LGPL"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on
  ("alexandria"
   "bordeaux-threads"
   "cffi"
   "trivial-garbage")
  :in-order-to ((test-op (test-op "cl-starpu-test-suite")))
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "swig-lispify")
   (:cffi-grovel-file "grovel")
   (:file "swig-interface")
   (:file "starpu")
   (:file "memory-node")
   (:file "worker")
   (:file "data")
   (:file "vector")
   (:file "matrix")
   (:file "block")
   (:file "codelet")
   (:file "task")))
