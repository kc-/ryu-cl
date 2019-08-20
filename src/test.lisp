(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :fiveam)
    (ql:quickload :fiveam)))

(defpackage :ryu-cl-tests (:use :common-lisp :fiveam))

(in-package #:ryu-cl-tests)

(def-test float-to-string-floats-to-string ()
  (is (string= "0.0" (ryu-cl:float-to-string 0.0)))
  (is (string= "23.42" (ryu-cl:float-to-string 23.42)))
  (is (string= "6.02214076d23" (ryu-cl:float-to-string 6.02214076d23)))
  (flet ((correctly-converted (float-number float-string)
           (string= (ryu-cl:float-to-string float-number) float-string)))
    (is-every
        correctly-converted
      (1.0e-5      "1.0e-5")
      (0.4321      "0.4321")
      (0.321       "0.321")
      (0.21        "0.21")
      (0.1         "0.1")
      (1.0         "1.0")
      (12.0        "12.0")
      (12.3        "12.3")
      (123.4       "123.4")
      (1234.5      "1234.5")
      (12345.6     "12345.6")
      (1.2345678e7 "1.2345678e7")
      (1.0e7       "1.0e7"))))
