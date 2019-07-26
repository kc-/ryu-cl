(defpackage :ryu-cl-tests (:use :common-lisp :fiveam))

(in-package #:ryu-cl-tests)

(def-test float-to-string-floats-to-string ()
  (is (string= "0.0" (ryu-cl:float-to-string 0.0)))
  (is (string= "23.42" (ryu-cl:float-to-string 23.42)))
  (is (string= "6.02214076d23" (ryu-cl:float-to-string 6.02214076d23))))
