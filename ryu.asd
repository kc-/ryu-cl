(in-package #:asdf-user)

(defsystem :ryu-cl
  :description "An implementation of the Ryu float printer by Ulf Adams."

  :depends-on ()

  :components ((:file "src/ryu")
               (:file "src/test")))
