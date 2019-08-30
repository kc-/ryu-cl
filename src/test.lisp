(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :fiveam)
    (ql:quickload :fiveam)))

(defpackage :ryu-cl/tests (:use :common-lisp :fiveam))

(in-package #:ryu-cl/tests)


(def-suite single-float-output)
(in-suite single-float-output)

(def-test float-to-string-basic-cases ()
  (is (string= "0.0" (ryu-cl:float-to-string 0.0)))
  (is (string= "-0.0" (ryu-cl:float-to-string -0.0)))
  (is (string= "1.0" (ryu-cl:float-to-string 1.0)))
  (is (string= "-1.0" (ryu-cl:float-to-string -1.0)))
  (is (string= "23.42" (ryu-cl:float-to-string 23.42))))

(def-test float-to-string-subnormal-numbers ()
  (is (string= "1.0e-38"     (ryu-cl:float-to-string 1.0e-38)))
  (is (string= "1.23456e-38" (ryu-cl:float-to-string 123.456e-40))))

(def-test float-to-string-clhs-22.1.3.1.3-printing-floats ()
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

(def-suite single-float-support-functions)
(in-suite single-float-support-functions)

(def-test multiple-of-power-of-5 ()
  (is (ryu-cl::multiple-of-power-of-5 1 0))
  (is (not (ryu-cl::multiple-of-power-of-5 1 1)))
  (is (ryu-cl::multiple-of-power-of-5 5 1))
  (is (ryu-cl::multiple-of-power-of-5 25 2))
  (is (ryu-cl::multiple-of-power-of-5 75 2))
  (is (ryu-cl::multiple-of-power-of-5 50 2))
  (is (not (ryu-cl::multiple-of-power-of-5 51 2)))
  (is (not (ryu-cl::multiple-of-power-of-5 75 4))))

(def-test multiple-of-power-of-2 ()
  (is (ryu-cl::multiple-of-power-of-2 1 0))
  (is (not (ryu-cl::multiple-of-power-of-2 1 1)))
  (is (ryu-cl::multiple-of-power-of-2 2 1))
  (is (ryu-cl::multiple-of-power-of-2 4 2))
  (is (ryu-cl::multiple-of-power-of-2 8 2))
  (is (ryu-cl::multiple-of-power-of-2 12 2))
  (is (not (ryu-cl::multiple-of-power-of-2 13 2)))
  (is (not (ryu-cl::multiple-of-power-of-2 8 4))))
