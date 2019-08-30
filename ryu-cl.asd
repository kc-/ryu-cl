(in-package #:asdf-user)

(defsystem "ryu-cl"
  :description "An implementation of the Ryu float printer by Ulf Adams."
  :components ((:file "src/ryu")))

(defsystem "ryu-cl/test"
  :description "Tests for the supporting functions and output of ryu-cl."
  :depends-on ("ryu-cl"
               (:version "fiveam" "1.4"))
  :components ((:file "src/test"))
  :perform (test-op (o s) (symbol-call :ryu-cl-test :run-all-tests)))
