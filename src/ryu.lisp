;;; Ported from C to Common Lisp.
;;; Source: https://github.com/ulfjack/ryu
;;;
;;; Copyright 2018 Ulf Adams
;;;
;;; The contents of this file may be used under the terms of the Apache License,
;;; Version 2.0.
;;;
;;;    (See accompanying file LICENSE-Apache or copy at
;;;     http://www.apache.org/licenses/LICENSE-2.0)
;;;
;;; Alternatively, the contents of this file may be used under the terms of
;;; the Boost Software License, Version 1.0.
;;;    (See accompanying file LICENSE-Boost or copy at
;;;     https://www.boost.org/LICENSE_1_0.txt)
;;;
;;; Unless required by applicable law or agreed to in writing, this software
;;; is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;; KIND, either express or implied.

(defpackage :ryu-cl
  (:documentation "An implementation of the Ryu float to string converter by Ulf Adams: https://github.com/ulfjack/ryu .")
  (:use :common-lisp)
  (:nicknames :ryu)
  (:export #:float-to-string))

(in-package :ryu-cl)

(defconstant +float-pow5-inv-bitcount+ 59)
(unless (boundp '+float-pow5-inv-split+)
  (defconstant +float-pow5-inv-split+ (make-array '(31) :element-type '(unsigned-byte 64)
                                                  :initial-contents
                                                 #(576460752303423489 461168601842738791 368934881474191033 295147905179352826
                                                   472236648286964522 377789318629571618 302231454903657294 483570327845851670
                                                   386856262276681336 309485009821345069 495176015714152110 396140812571321688
                                                   316912650057057351 507060240091291761 405648192073033409 324518553658426727
                                                   519229685853482763 415383748682786211 332306998946228969 531691198313966350
                                                   425352958651173080 340282366920938464 544451787073501542 435561429658801234
                                                   348449143727040987 557518629963265579 446014903970612463 356811923176489971
                                                   570899077082383953 456719261665907162 365375409332725730))))

(defconstant +float-pow5-bitcount+ 61)
(unless (boundp '+float-pow5-split+)
  (defconstant +float-pow5-split+ (make-array '(47) :element-type '(unsigned-byte 64)
                                              :initial-contents
                                             #(1152921504606846976 1441151880758558720 1801439850948198400 2251799813685248000
                                               1407374883553280000 1759218604441600000 2199023255552000000 1374389534720000000
                                               1717986918400000000 2147483648000000000 1342177280000000000 1677721600000000000
                                               2097152000000000000 1310720000000000000 1638400000000000000 2048000000000000000
                                               1280000000000000000 1600000000000000000 2000000000000000000 1250000000000000000
                                               1562500000000000000 1953125000000000000 1220703125000000000 1525878906250000000
                                               1907348632812500000 1192092895507812500 1490116119384765625 1862645149230957031
                                               1164153218269348144 1455191522836685180 1818989403545856475 2273736754432320594
                                               1421085471520200371 1776356839400250464 2220446049250313080 1387778780781445675
                                               1734723475976807094 2168404344971008868 1355252715606880542 1694065894508600678
                                               2117582368135750847 1323488980084844279 1654361225106055349 2067951531382569187
                                               1292469707114105741 1615587133892632177 2019483917365790221))))

(defconstant +uint32-max+ #xFFFFFFFF)

(defun pow5-bits (e)
  "Returns e == 0 ? 1 : ceil(log_2(5^e))."
  (declare (type (signed-byte 32) e))
  ;; approximation works up to the point that the multiplication overflows at e = 3529. If the multiplication were done in 64 bits, it would fail at 5^4004 which is just greater than 2^9297.
  (assert (<= 0 e))
  (assert (<= e 3528))
  (the (signed-byte 32) (1+ (ash (* e 1217359) -19))))

(defun log10-pow2 (e)
  "Return floor(log_10(2^e))."
  (declare (type (signed-byte 32) e)
           (optimize (speed 3) (safety 0) (debug 0)))
  (assert (<= 0 e))
  (assert (<= e 1650))
  (the (signed-byte 32) (ash (* e 78913) -18)))

(defun log10-pow5 (e)
  "Return floor(log_10(5^e))."
  (declare (type (signed-byte 32) e))
  (assert (<= 0 e))
  (assert (<= e 2620))
  (the (signed-byte 32) (ash (* e 732923) -20)))

(defun mul-shift (m factor shift)
  (declare (type (unsigned-byte 32) m shift)
           (type (unsigned-byte 64) factor))
  (assert (> shift 32))

  (let* ((factor-lo factor)
         (factor-hi (ash factor -32))
         (bits-0 (* m factor-lo))
         (bits-1 (* m factor-hi))
         (sum (+ (ash bits-0 -32) bits-1))
         (shifted-sum (ash sum (- 32 shift))))
    (assert (<= shifted-sum +uint32-max+))
    (the (unsigned-byte 32) shifted-sum)))

(defun mul-pow5-inv-div-pow2 (m i j)
  (declare (type (unsigned-byte 32) m i)
           (type (signed-byte 32) j))
  (mul-shift m (aref +float-pow5-inv-split+ i) j))

(defun mul-pow5-div-pow2 (m i j)
  (declare (type (unsigned-byte 32) m i)
           (type (signed-byte 32) j))
  (mul-shift m (aref +float-pow5-split+ i) j))

(defun pow5-factor (value)
  (declare (type (unsigned-byte 32) value))
  (let ((count 0))
    (loop
     (multiple-value-bind (q r)
         (truncate value 5)
       (assert (not (zerop value)))
       (unless (zerop r) (return-from pow5-factor count))
       (setf value q)
       (incf count)))))

(defun multiple-of-power-of-5 (value p)
  (declare (type (unsigned-byte 32) value p))
  (>= (pow5-factor value) p))

(defun multiple-of-power-of-2 (value p)
  (declare (type (unsigned-byte 32) value p))
  (zerop (logand value (1- (ash 1 p)))))

(defun compute-q-vr-vp-vm (mv mm mp e2 accept-bounds mm-shift)
  (let ((vr-is-trailing-zeros nil)
        (vm-is-trailing-zeros nil))
   (if (minusp e2)
       (let* ((last-removed-digit nil)
              (q (log10-pow5 (- e2)))
              (e10 (+ q e2))
              (i (- 0 e2 q))
              (k (- (pow5-bits i) +float-pow5-bitcount+))
              (j (- q k))
              (vr (mul-pow5-div-pow2 mv q i))
              (vp (mul-pow5-div-pow2 mp q i))
              (vm (mul-pow5-div-pow2 mm q i)))
         (when (and (not (zerop q))
                    (<= (/ (1- vp) 10)
                        (/ vm 10)))
           (setf j (- q 1 (- (pow5-bits (1+ i)) +float-pow5-bitcount+))
                 last-removed-digit (mod (mul-pow5-div-pow2 mv (1+ i) j) 10)))
         (cond ((<= q 1)
                (setf vr-is-trailing-zeros t)
                (if accept-bounds
                    (setf vm-is-trailing-zeros (eql mm-shift 1))
                    (decf vp)))
               ((< q 31)
                (setf vr-is-trailing-zeros (multiple-of-power-of-2 mv (1- q))))))
       (let* ((q (log10-pow2 e2))
              (k (+ +float-pow5-bitcount+ (pow5-bits q) -1))
              (i (+ (- e2) q k))
              (vr (mul-pow5-inv-div-pow2 mv q i))
              (vp (mul-pow5-inv-div-pow2 mp q i))
              (vm (mul-pow5-inv-div-pow2 mm q i)))
         (when (and (not (zerop q))
                    (<= (/ (1- vp) 10)
                        (/ vm 10)))
           (let* ((l (+ +float-pow5-inv-bitcount+ (pow5-bits (1- q)) -1))
                  (last-removed-digit
                   (mod (mul-pow5-inv-div-pow2 mv (1- q) (+ (- e2) q -1 l)) 10)))
             (when (<= q 9)
               (cond
                 ((zerop (mod mv 5))
                  (setf vr-is-trailing-zeros (multiple-of-power-of-5 mv q)))
                 (accept-bounds
                  (setf vm-is-trailing-zeros (multiple-of-power-of-5 mm q)))
                 ((multiple-of-power-of-5 mp q) (decf vp))))
             (values q vr vp vm last-removed-digit)))))))

(defmethod float-to-string (float-number)
  (multiple-value-bind (significand exponent sign)
      (integer-decode-float float-number)
    (cond
      ((= 0 float-number)
       (return-from float-to-string
         (if (minusp sign) "-0.0" "0.0")))
      ((or (and (typep float-number 'single-float)
                (= exponent #xFF))
           (= exponent #x7FF))
       (return-from float-to-string (if (zerop significand)
                                        (if (minusp sign) "-Infinity" "Infinity")
                                        "NaN"))))
    (let* ((e2 (- exponent 2))
           (u (* (- (* 4 significand) 2)))
           (v significand)
           (w (* (+ (* 4 significand) 2)))
           (e10 (if (minusp e2) e2 0))
           (decimal-factor (if (minusp e2) (expt 5 (- e2)) (expt 2 e2))))
      (multiple-value-bind (dec-coeff dec-exponent)
          (compute-shortest (* decimal-factor u) (* decimal-factor v) (* decimal-factor w))
        (let* ((digits (princ-to-string dec-coeff))
               (final-exponent (1- (+ dec-exponent e10 (length digits)))))
          (with-output-to-string (s)
            (when (minusp sign)
              (princ "-" s))
            (cond
              ((< -4 final-exponent 7)
               (cond ((plusp final-exponent)
                      (princ (subseq digits 0 (min (length digits) (1+ final-exponent))) s)
                      (let ((n-zeros (1+ (- final-exponent (length digits)))))
                        (when (plusp n-zeros)
                          (princ (subseq "000" 0 n-zeros) s)))
                      (princ #\. s)
                      (if (> (length digits) (1+ final-exponent))
                          (princ (subseq digits (1+ final-exponent)) s)
                          (princ "0" s)))
                     ((minusp final-exponent)
                      (princ (subseq "0.00" 0 (1+ (abs final-exponent))) s)
                      (princ digits s))
                     (T (princ digits s)
                        (princ ".0" s))))
              (T
               (when (minusp sign) (princ #\- s))
               (princ (elt digits 0) s)
               (princ #\. s)
               (princ (if (> (length digits) 1) (subseq digits 1) "0") s)
               (princ (if (typep float-number 'single-float) #\e #\d) s)
               (princ (1- (+ dec-exponent e10 (length digits))) s)))
            s))))))

(defun compute-shortest (a b c &optional (accept-smaller T) (accept-larger T) (break-tie-down nil))
  "For a float value in the interval [A,C], compute the shortest decimal representation."
  (let ((a a) (b b) (c (if accept-larger c (1- c)))
        (digit 0)
        (all-b-zero T)
        (i 0)
        (all-a-zero T))
    (loop
       for a-floor = (floor a 10)
       for c-floor = (floor c 10)
       while (< a-floor c-floor)
       do
         (setf all-a-zero (and all-a-zero (zerop (mod a 10))))
         (setf a a-floor)
         (setf c c-floor)
         (setf digit (mod b 10))
         (setf all-b-zero (and all-b-zero (zerop digit)))
         (incf i))
    (when (and accept-smaller all-a-zero)
      (loop
         while (zerop (mod a 10))
         do
           (setf a (/ a 10))
           (setf c (truncate c 10))
           (incf i)))
    (let* ((is-tie (and all-b-zero (= 5 digit)))
           (want-round-down (or (< digit 5)
                                (and is-tie break-tie-down)))
           (round-down (or (and want-round-down (or (not (= a b))
                                                    all-a-zero))
                           (> (1+ b) c))))

      (values (if round-down c (1+ c)) i))))

(defun minmax-naive (a b M)
  (let* ((result-min (mod a b))
         (result-max result-min))
    (loop for i from 2 to M
       for cur = (mod (* i a) b)
       if (< cur result-min) do (setf result-min cur)
       if (< result-max cur) do (setf result-max cur))
    (values result-min result-max)))

(defun minmax-euclid (a b M)
  (let ((_a a)
        (_b b)
        (_s 1)
        (_t 0)
        (_u 0)
        (_v 1))
    (loop
       (loop while (>= _b _a) do
            (decf _b _a)
            (decf _u _s)
            (decf _v _t)
            (assert (= _a (+ (* a _s) (* b _t))))
            (assert (= _b (+ (* a _u) (* b _v))))
            (when (>= (- _u) M)
              (return-from minmax-euclid (values _a _b))))

       (when (zerop _b)
         (return-from minmax-euclid (values 1 (1- b))))

       (loop while (>= _a _b) do
            (decf _a _b)
            (decf _s _u)
            (decf _t _v)
            (assert (= _a (+ (* a _s) (* b _t))))
            (assert (= _b (+ (* a _u) (* b _v))))
            (when (>= _s M)
              (return-from minmax-euclid (values _a _b))))

       (when (zerop _a)
         (return-from minmax-euclid (values 1 (1- b)))))))
