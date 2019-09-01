(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :fiveam)
    (ql:quickload :fiveam)))

(defpackage :ryu-cl/tests (:use :common-lisp :fiveam))

(in-package #:ryu-cl/tests)

(def-suite ryu-test-suite)

(def-suite ryu-double-float-output :in ryu-test-suite)
(in-suite ryu-double-float-output)


(def-test double-to-string-basic-cases ()
  (is (string= "0.0d0" (ryu-cl:double-float-to-string 0.0d0)))
  (is (string= "-0.0d0" (ryu-cl:double-float-to-string -0.0d0)))
  (is (string= "1.0d0" (ryu-cl:double-float-to-string 1.0d0)))
  (is (string= "-1.0d0" (ryu-cl:double-float-to-string -1.0d0)))
  (is (string= "23.42d0" (ryu-cl:double-float-to-string 23.42d0))))

(def-test double-to-string-clhs-22.1.3.1.3-printing-floats ()
  (flet ((correctly-converted (float-number float-string)
           (string= (ryu-cl:double-float-to-string float-number) float-string)))
    (is-every
        correctly-converted
      (1.0d-5      "1.0d-5")
      (0.4321d0    "0.4321d0")
      (0.321d0     "0.321d0")
      (0.21d0      "0.21d0")
      (0.1d0       "0.1d0")
      (1.0d0       "1.0d0")
      (12.0d0      "12.0d0")
      (12.3d0      "12.3d0")
      (123.4d0     "123.4d0")
      (1234.5d0    "1234.5d0")
      (12345.6d0   "12345.6d0")
      (1.2345678d7 "1.2345678d7")
      (1.0d7       "1.0d7"))))

(def-suite ryu-single-float-output :in ryu-test-suite)
(in-suite ryu-single-float-output)


(def-test single-float-to-string-basic-cases ()
  (is (string= "0.0" (ryu-cl:float-to-string 0.0)))
  (is (string= "-0.0" (ryu-cl:float-to-string -0.0)))
  (is (string= "1.0" (ryu-cl:float-to-string 1.0)))
  (is (string= "-1.0" (ryu-cl:float-to-string -1.0)))
  (is (string= "23.42" (ryu-cl:float-to-string 23.42))))

(def-test single-float-to-string-subnormal-numbers ()
  (is (string= "1.0e-38"     (ryu-cl:float-to-string 1.0e-38)))
  (is (string= "1.23456e-38" (ryu-cl:float-to-string 123.456e-40))))

(def-test single-float-to-string-clhs-22.1.3.1.3-printing-floats ()
  (flet ((correctly-converted (float-number float-string)
           (string= (ryu-cl:single-float-to-string float-number) float-string)))
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
