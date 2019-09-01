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
  (:export #:float-to-string
           #:single-float-to-string #:double-float-to-string))

(in-package :ryu-cl)

(defmacro dbg (&rest symbols)
  (when (boundp 'RYU-DEBUG)
    `(format *debug-io*
             ,(format nil "~{~a:~~a ~}~~%" (mapcar #'string-downcase symbols))
             ,@symbols)))

(defconstant +ieee-single-float-mantissa-bit-length+ 23)
(defconstant +ieee-single-float-exponent-bit-length+  8)

(defconstant +ieee-double-float-mantissa-bit-length+ 53)
(defconstant +ieee-double-float-exponent-bit-length+ 11)

;;; ----------------------------------------------------------------------------
;;; single-float tables
;;;
(defconstant +float-pow5-inv-bitcount+ 59)
(defconstant +float-pow5-inv-table-size+ 31)
(defun make-float-pow5-inv-lookup-table ()
  (loop with lookup-table = (make-array `(,+float-pow5-inv-table-size+) :element-type '(unsigned-byte 64))
        for i below +float-pow5-inv-table-size+
        for pow = (expt 5 i)
        for pow5len = (1+ (floor (log pow 2)))
        for j = (+ pow5len -1 +float-pow5-inv-bitcount+)
        for v = (1+ (truncate (ash 1 j) pow))
        do (setf (aref lookup-table i) v)
        finally (return lookup-table)))

(unless (boundp '+float-pow5-inv-split+)
  (defconstant +float-pow5-inv-split+
    (make-float-pow5-inv-lookup-table)))

(defconstant +float-pow5-bitcount+ 61)
(defconstant +float-pow5-table-size+ 47)

(defun make-float-pow5-lookup-table ()
  (loop with lookup-table = (make-array `(,+float-pow5-table-size+))
        for i below +float-pow5-table-size+
        for pow = (expt 5 i)
        for pow5len = (1+ (floor (log pow 2)))
        for v = (ash pow (- +float-pow5-bitcount+ pow5len))
        do (setf (aref lookup-table i) v)
        finally (return lookup-table)))

(unless (boundp '+float-pow5-split+)
  (defconstant +float-pow5-split+
    (make-float-pow5-lookup-table)))

(defconstant +uint32-max+ #xFFFFFFFF)

(declaim (inline pow5-bits
                 log10-pow2 log10-pow5
                 mul-shift
                 mul-pow5-div-pow2 mul-pow5-inv-div-pow2
                 ieee-float-bias ieee-float-mantissa-bits
                 multiple-of-power-of-2-32 multiple-of-power-of-5-64 pow5-factor-32
                 multiple-of-power-of-2-64 multiple-of-power-of-5-64 pow5-factor-64))

(defun pow5-bits (e)
  "Returns e == 0 ? 1 : ceil(log_2(5^e))."
  (declare (type (signed-byte 32) e))
  ;; approximation works up to the point that the multiplication overflows at e = 3529.
  ;; If the multiplication were done in 64 bits, it would fail at 5^4004 which is just greater than 2^9297.
  (assert (<= 0 e 3528))
  (1+ (ash (* e 1217359) -19)))

(defun log10-pow2 (e)
  "Return floor(log_10(2^e))."
  (declare (type (signed-byte 32) e)
           (optimize (speed 3) (safety 0) (debug 0)))
  (assert (<= 0 e 1650))
  (the (signed-byte 32) (ash (* e 78913) -18)))

(defun log10-pow5 (e)
  "Return floor(log_10(5^e))."
  (declare (type (signed-byte 32) e))
  (assert (<= 0 e 2620))
  (the (signed-byte 32) (ash (* e 732923) -20)))

(defun mul-shift (m factor shift)
  (declare (type (unsigned-byte 32) m shift)
           (type (unsigned-byte 64) factor))
  (assert (> shift 32))

  (let* ((factor-lo (ldb (byte 32 0) factor))
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

(defun pow5-factor-32 (value)
  (declare (type (unsigned-byte 32) value))
  (let ((count 0))
    (loop
       (multiple-value-bind (q r)
           (truncate value 5)
         (assert (not (zerop value)))
         (unless (zerop r) (return-from pow5-factor-32 count))
         (setf value q)
         (incf count)))))

(defun pow5-factor-64 (value)
  (declare (type (unsigned-byte 32) value))
  (let ((count 0))
    (loop
       (multiple-value-bind (q r)
           (truncate value 5)
         (assert (not (zerop value)))
         (unless (zerop r) (return-from pow5-factor-64 count))
         (setf value q)
         (incf count)))))

(defun multiple-of-power-of-5-32 (value p)
  (declare (type (unsigned-byte 32) value p))
  (>= (pow5-factor-32 value) p))

(defun multiple-of-power-of-2-32 (value p)
  (declare (type (unsigned-byte 32) value p))
  (zerop (logand value (1- (ash 1 p)))))

(defun multiple-of-power-of-5-64 (value p)
  (declare (type (unsigned-byte 63) value p))
  (>= (pow5-factor-64 value) p))

(defun multiple-of-power-of-2-64 (value p)
  (declare (type (unsigned-byte 64) value p))
  (zerop (logand value (1- (ash 1 p)))))

(defun compute-decimal-interval (mm mv mp e2 accept-bounds mm-shift)
  (let ((vr-is-trailing-zeros nil)
        (vm-is-trailing-zeros nil)
        (last-removed-digit 0)
        (q (if (minusp e2)
               (log10-pow5 (- e2))
               (log10-pow2 e2))))
    (if (minusp e2)
        (let* ((e10 (+ q e2))
               (i (- 0 e2 q))
               (k (- (pow5-bits i) +float-pow5-bitcount+))
               (j (- q k))
               (vr (mul-pow5-div-pow2 mv i j))
               (vp (mul-pow5-div-pow2 mp i j))
               (vm (mul-pow5-div-pow2 mm i j)))
          (dbg mm mv mp)
          (dbg q i j k)
          (when (and (not (zerop q))
                     (<= (truncate (1- vp) 10)
                         (truncate vm 10)))
            (setf j (- q 1 (- (pow5-bits (1+ i)) +float-pow5-bitcount+))
                  last-removed-digit (mod (mul-pow5-div-pow2 mv (1+ i) j) 10)))
          (cond ((<= q 1)
                 (setf vr-is-trailing-zeros t)
                 (if accept-bounds
                     (setf vm-is-trailing-zeros (eql mm-shift 1))
                     (decf vp)))
                ((< q 31)
                 (setf vr-is-trailing-zeros (multiple-of-power-of-2-32 mv (1- q)))))
          (values e10 vr vp vm last-removed-digit vm-is-trailing-zeros vr-is-trailing-zeros))
        (let* ((e10 q)
               (k (+ +float-pow5-inv-bitcount+ (pow5-bits q) -1))
               (i (+ (- e2) q k))
               (vr (mul-pow5-inv-div-pow2 mv q i))
               (vp (mul-pow5-inv-div-pow2 mp q i))
               (vm (mul-pow5-inv-div-pow2 mm q i)))
          (dbg mm mv mp)
          (dbg q i k)
          (when (and (not (zerop q))
                     (<= (truncate (1- vp) 10)
                         (truncate vm 10)))
            (let* ((l (+ +float-pow5-inv-bitcount+ (pow5-bits (1- q)) -1)))
              (setf last-removed-digit (mod (mul-pow5-inv-div-pow2 mv (1- q) (+ (- e2) q -1 l)) 10))
              (when (<= q 9)
                (cond
                  ((zerop (mod mv 5))
                   (setf vr-is-trailing-zeros (multiple-of-power-of-5-32 mv q)))
                  (accept-bounds
                   (setf vm-is-trailing-zeros (multiple-of-power-of-5-32 mm q)))
                  ((multiple-of-power-of-5-32 mp q) (decf vp))))))
          (values e10 vr vp vm last-removed-digit vm-is-trailing-zeros vr-is-trailing-zeros)))))

(defun ieee-float-bias (float-number)
  (etypecase float-number
    (single-float 127)
    (double-float 1023)))

(defun ieee-float-mantissa-bits (float-number)
  (etypecase float-number
    (single-float (sb-kernel:single-float-bits float-number))
    (double-float (sb-kernel:double-float-bits float-number))))

(defun single-float-to-string (float-number)
  (multiple-value-bind (significand exponent sign)
      (integer-decode-float float-number)
    (when (zerop float-number)            ; bail out early -- there's no infinity
      (return-from single-float-to-string ; or nan for common lisp floats,
        (with-output-to-string (s)        ; should those be part of this, too?
          (when (minusp sign)
            (princ #\- s))
          (princ "0.0" s)
          (unless (typep float-number *read-default-float-format*)
            (princ (if (typep float-number 'single-float) "f0" "d0") s))
          s)))
    ;; TODO: don't rely on sbcl internals for this
    (when (and (sb-ext:float-denormalized-p float-number)
               (evenp significand))
      (setf significand (ieee-float-mantissa-bits float-number))
      (setf exponent (- 1 (ieee-float-bias float-number) +ieee-single-float-mantissa-bit-length+)))
    (let* ((e2 (- exponent 2))
           (accept-bounds (evenp significand))
           (ieee-zero-mantissa (etypecase float-number
                                 (single-float #x800000)
                                 (double-float #x10000000000000)))
           (mm-shift (if (or (not (= ieee-zero-mantissa significand))
                             (<= exponent (+ 1 (floor (log significand 2))
                                             (ieee-float-bias float-number))))
                         1
                         0))
           (u (* (- (* 4 significand) 1 mm-shift)))
           (v (* 4 significand))

           (w (* (+ (* 4 significand) 2)))
           (removed-digit-count 0)
           (output))
      (dbg  e2 mm-shift u v w)
      (multiple-value-bind (e10 vr vp vm last-removed-digit vm-is-trailing-zeros vr-is-trailing-zeros)
          (compute-decimal-interval u v w e2 accept-bounds (or (not (zerop significand)) (<= exponent 1)))
        (dbg e10 e2 vr vp vm last-removed-digit vm-is-trailing-zeros vr-is-trailing-zeros)
        (cond
          ((or vm-is-trailing-zeros vr-is-trailing-zeros)
           (dbg vm-is-trailing-zeros vr-is-trailing-zeros)
           (loop while (> (truncate vp 10) (truncate vm 10)) do
             (setf vm-is-trailing-zeros (and vm-is-trailing-zeros (zerop (mod vm 10)))
                   vr-is-trailing-zeros (and vr-is-trailing-zeros (zerop last-removed-digit))
                   vr (truncate vr 10)
                   vp (truncate vp 10)
                   vm (truncate vm 10))
             (incf removed-digit-count))

           (when vm-is-trailing-zeros
             (loop while (zerop (mod vm 10)) do
               (setf last-removed-digit (mod vr 10)
                     vr (truncate vr 10)
                     vp (truncate vp 10)
                     vm (truncate vm 10))
               (incf removed-digit-count)))

           (when (and vr-is-trailing-zeros (= last-removed-digit 5) (zerop (mod vr 2)))
             (setf last-removed-digit 4))

           (setf output vr)
           (when (or
                  (and (eql vr vm)
                       (or (not accept-bounds)
                           (not vm-is-trailing-zeros)))
                  (>= last-removed-digit 5))
             (incf vr)))
          (T
           (loop named remove-digits-loop do
             (multiple-value-bind (vr-truncated vr-last-digit)
                 (truncate vr 10)
               (let ((vm-truncated (truncate vm 10))
                     (vp-truncated (truncate vp 10)))
                 (when (<= vp-truncated vm-truncated)
                   (return-from remove-digits-loop))
                 (setf last-removed-digit vr-last-digit
                       vr vr-truncated
                       vp vp-truncated
                       vm vm-truncated)
                 (incf removed-digit-count))))
           (dbg vr last-removed-digit)
           (when (or (eql vr vm) (>= last-removed-digit 5))
             (incf vr))
           (setf output vr)))
        (digits-as-string sign output e10 removed-digit-count 'single-float)))))

(defun digits-as-string (sign output e10 removed-digit-count float-type)
  (let ((exp (+ e10 removed-digit-count)))
    (when (minusp sign) (princ #\-))
    (let* ((digits (princ-to-string output))
           (final-exponent (1- (+ (length digits) e10 removed-digit-count))))
      (dbg digits final-exponent e10 removed-digit-count)
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
           (if (eql float-type *read-default-float-format*)
               (princ #\e s)
               (princ (if (eql float-type 'single-float) #\f #\d) s))
           (princ (1- (+ exp (length digits))) s)))
        s))))
;;; ----------------------------------------------------------------------------
;;; double-float tables
;;;
(unless (boundp '+double-pow5-inv-table-size+)
  (defconstant +double-pow5-inv-table-size+ `(292 2)))
(unless (boundp '+double-pow5-table-size+)
  (defconstant +double-pow5-table-size+ `(326 2)))

(defconstant +double-pow5-inv-bitcount+ 122)
(defconstant +double-pow5-bitcount+ 121)

(defun make-double-pow5-inv-lookup-table ()
  (loop with lookup-table = (make-array +double-pow5-inv-table-size+
                                        :element-type '(unsigned-byte 128))
        for i from 0 below (car +double-pow5-inv-table-size+)
        for pow = (expt 5 i)
        for pow5len = (1+ (floor (log pow 2)))
        for j = (+ pow5len -1 +double-pow5-inv-bitcount+)
        for inv = (1+ (truncate (ash 1 j) pow))
        do
           (setf (aref lookup-table i 0) (ldb (byte 64  0) inv)
                 (aref lookup-table i 1) (ldb (byte 64 64) inv))
        finally (return lookup-table)))

(unless (boundp '+double-pow5-inv-split+)
  (defconstant +double-pow5-inv-split+
    (make-double-pow5-inv-lookup-table)))


(defun make-double-pow5-lookup-table ()
  (loop
    with lookup-table = (make-array +double-pow5-table-size+
                                    :element-type '(unsigned-byte 64))
    for i from 0 below (car +double-pow5-table-size+)
    for pow = (expt 5 i)
    for pow5len = (1+ (floor (log pow 2)))
    for pow5High = (ash pow (- +double-pow5-bitcount+ pow5len 64))
    for  pow5Low = (ldb (byte 64 0) (ash pow (- +double-pow5-bitcount+ pow5len)))
    do
       (setf (aref lookup-table i 0) pow5low
             (aref lookup-table i 1) pow5High)
    finally (return lookup-table)))

(unless (boundp '+double-pow5-split+)
  (defconstant +double-pow5-split+
    (make-double-pow5-lookup-table)))

(declaim (inline mul-shift-128 mul-shift-all))

(defun mul-shift-128 (m mul-0 mul-1 j)
  (declare (type (unsigned-byte 64) m mul-0 mul-1)
           (type (unsigned-byte 32) j))
  (let ((b0 (* m mul-0))
        (b1 (* m mul-1)))
    (the (unsigned-byte 64) (ash (+ (ash b0 -64) b1) (- 64 j)))))

(defun mul-shift-all (m mul-0 mul-1 j mm-shift)
  ;; (declare (type (unsigned-byte 64) m mul-0 mul-1)
  ;;          (type (unsigned-byte 32) j mm-shift))

  (dbg m mul-0 mul-1 j mm-shift)

  (let ((vp (mul-shift-128 (+ (* 4 m) 2) mul-0 mul-1 j))
        (vm (mul-shift-128 (- (* 4 m) 1 mm-shift) mul-0 mul-1 j)))
    (values (mul-shift-128 (* 4 m) mul-0 mul-1 j)
            vp
            vm)))

(defun double-float-to-string (double-number)
  (multiple-value-bind (significand exponent sign)
      (integer-decode-float double-number)
    (when (zerop double-number)           ; bail out early -- there's no infinity
      (return-from double-float-to-string ; or nan for common lisp floats,
        (with-output-to-string (s)        ; should those be part of this, too?
          (when (minusp sign)
            (princ #\- s))
          (princ "0.0" s)
          (unless (typep double-number *read-default-float-format*)
            (princ (if (typep double-number 'single-float) "f0" "d0") s))
          s)))
    (when (and (sb-ext:float-denormalized-p double-number)
               (evenp significand))
      (setf significand (ieee-float-mantissa-bits double-number))
      (setf exponent (- 1 (ieee-float-bias double-number) +ieee-double-float-mantissa-bit-length+)))

    (let* ((e2 (- exponent 2))
           (mv (* 4 significand))
           (accept-bounds (evenp significand))
           (ieee-zero-mantissa (etypecase double-number
                                 (single-float #x800000)
                                 (double-float #x10000000000000)))
           (mm-shift (if (or (not (= ieee-zero-mantissa significand))
                             (<= exponent (+ 1 (floor (log significand 2))
                                             (ieee-float-bias double-number))))
                         1
                         0))
           (vm-is-trailing-zeros nil) (vr-is-trailing-zeros nil)
           (e10 nil) (vr nil) (vp nil) (vm nil))
      (dbg significand e2 mm-shift)
      (if (minusp e2)
          (let* ((q (log10-pow5 (- 0 e2 (if (> (- e2) 1) 1 0))))
                 (i (- 0 e2 q))
                 (k (- (pow5-bits i) +double-pow5-bitcount+))
                 (j (- q k))
                 (pow5-hi (aref +double-pow5-split+ i 0))
                 (pow5-lo (aref +double-pow5-split+ i 1)))
            (setf e10 (+ q e2))
            (dbg q i k j)
            (multiple-value-setq (vr vp vm)
              (mul-shift-all significand pow5-hi pow5-lo j mm-shift))
            (dbg vp vr vm)
            (cond
              ((<= q 1)
               (setf vr-is-trailing-zeros T)
               (if accept-bounds
                   (setf vm-is-trailing-zeros (= 1 mm-shift))
                   (decf vp)))
              ((< q 63)
               (setf vr-is-trailing-zeros (multiple-of-power-of-2-64 mv q)))))
          (let* ((q (- (log10-pow2 e2) (if (> e2 3) 1 0)))
                 (k (+ +double-pow5-inv-bitcount+ (pow5-bits q) -1))
                 (i (+ (- e2) q k))
                 (pow5-hi (aref +double-pow5-inv-split+ i 0))
                 (pow5-lo (aref +double-pow5-inv-split+ i 1)))
            (multiple-value-setq (vr vp vm)
              (mul-shift-all significand pow5-hi pow5-lo i mm-shift))
            (when (<= q 21)
              (cond
                ((zerop (mod mv 5))
                 (setf vr-is-trailing-zeros (multiple-of-power-of-5-64 mv q)))
                (accept-bounds
                 (setf vm-is-trailing-zeros (multiple-of-power-of-5-64 (- mv 1 mm-shift) q)))
                ((multiple-of-power-of-5-64 (+ mv 2) q)
                 (decf vp))))))
      (let ((removed-digit-count 0)
            (last-removed-digit 0))
        (dbg vm-is-trailing-zeros vr-is-trailing-zeros)
        (cond
          ((or vm-is-trailing-zeros vr-is-trailing-zeros)
           (loop
              (let ((vp-div-10 (truncate vp 10)))
               (multiple-value-bind (vm-div-10 vm-mod-10)
                   (truncate vm 10)
                 (when (<= vp-div-10 vm-div-10)
                     (return))
                 (multiple-value-bind (vr-div-10 vr-mod-10)
                     (truncate vr 10)
                   (setf vm-is-trailing-zeros (and vm-is-trailing-zeros
                                                   (zerop vm-mod-10))
                         vr-is-trailing-zeros (and vr-is-trailing-zeros
                                                   (zerop last-removed-digit))
                         last-removed-digit vr-mod-10)

                   (setf vr vr-div-10
                         vp vp-div-10
                         vm vm-div-10)
                   (incf removed-digit-count)))))
           (when vm-is-trailing-zeros
             (loop
                (multiple-value-bind (vm-div-10 vm-mod-10)
                    (truncate vm 10)
                  (unless (zerop vm-mod-10)
                    (return))
                  (multiple-value-bind (vr-div-10 vr-mod-10)
                      (truncate vr 10)
                    (let ((vp-div-10 (truncate vp 10)))
                      (setf vr-is-trailing-zeros (and vr-is-trailing-zeros
                                                      (zerop last-removed-digit))
                            last-removed-digit vr-mod-10)
                      (setf vr vr-div-10
                            vp vp-div-10
                            vm vm-div-10)
                      (incf removed-digit-count))))))
           (when (and vr-is-trailing-zeros
                      (= 5 last-removed-digit)
                      (evenp vr))
             (setf last-removed-digit 4))
           (when (or (and (eql vr vm)
                          (or (not accept-bounds)
                              (not vm-is-trailing-zeros))
                          )
                     (>= last-removed-digit 5))
             (incf vr)))
          (T
           (dbg T)
           (let ((round-up-p nil)
                 (vp-div-100 (truncate vp 100))
                 (vm-div-100 (truncate vm 100)))
             (when (> vp-div-100 vm-div-100)
               (multiple-value-bind (vr-div-100 vr-mod-100)
                   (truncate vr 100)
                 (setf round-up-p (>= vr-mod-100 50)
                       vr vr-div-100
                       vp vp-div-100
                       vm vm-div-100)
                 (incf removed-digit-count 2)
                 (dbg removed-digit-count vr)))
             (loop
                (let ((vp-div-10 (truncate vp 10))
                      (vm-div-10 (truncate vm 10)))
                  (when (<= vp-div-10 vm-div-10)
                    (return))
                  (multiple-value-bind (vr-div-10 vr-mod-10)
                      (truncate vr 10)
                    (setf round-up-p (>= vr-mod-10 5)
                          vr vr-div-10
                          vp vp-div-10
                          vm vm-div-10)
                    (incf removed-digit-count)
                    (dbg removed-digit-count vr)))
                (when (or (eql vr vm) round-up-p)
                  (incf vr))))))
        (digits-as-string sign vr e10 removed-digit-count 'double-float)))))

(defun float-to-string (float-number)
  (etypecase float-number
    (single-float (single-float-to-string float-number))
    (double-float (double-float-to-string float-number))))
