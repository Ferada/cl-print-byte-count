;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(in-package #:cl-user)

(defconstant si-prefixes "KMGTPEZY")

(defun calculate-byte-count (object &optional (base-ten-threshold T) base-ten-divisor strict-p (postfix #\B))
  (loop
     with abs = (abs object)
     for i from 1 and i-1 from 0 and j from -1
     for threshold = (if base-ten-threshold (expt 10 (* i 3)) (expt 2 (* i 10)))
     for divisor = (if base-ten-divisor (expt 10 (* i-1 3)) (expt 2 (* i-1 10)))
     for done = (eql i #.(length si-prefixes))
     when (or (< abs threshold) done)
     do (return (let ((result (/ object divisor))
                      (i-p (and strict-p (not base-ten-divisor) #\i)))
                  (if (eql i 1)
                      result
                      (values
                       result
                       (let ((char (char si-prefixes j)))
                         (if (and (not i-p)
                                  (char= char #\K))
                             #\k
                             char))
                       i-p
                       postfix))))
     until done))

(defun print-byte-count (stream object &optional base-two-threshold base-ten-divisor
                                         (round-digits 0) strict-p (postfix #\B))
  "If BASE-TWO-THRESHOLD is unset (default), use exponents of ten as
threshold, otherwise use exponents of two.  If BASE-TEN-DIVISOR is unset
\(default), use exponents of two as divisor, otherwise uses exponents of
ten.  ROUND-DIGITS (default 0) are the number of digits to be printed after
the decimal dot.  If STRICT-P is unset (default), the SI #\i isn't used for
exponents of two, so \"kB\" will be printed even if \"KiB\" would be more
correct.  If POSTFIX is specified (defaults to #\B), it will be printed
afterwards."
  (multiple-value-bind (number prefix middle postfix)
      (calculate-byte-count
       object
       (not base-two-threshold) base-ten-divisor
       strict-p postfix)
    (if (or (not round-digits) (<= round-digits 0))
        (format stream "~D" (round number))
        (format stream "~V$" round-digits number))
    (when prefix
      (write-char prefix stream))
    (when middle
      (write-char middle stream))
    (when postfix
      (write-char postfix stream)))
  NIL)

(fiveam:def-suite print-byte-count)
(fiveam:in-suite print-byte-count)

(fiveam:def-test test.1 ()
  (fiveam:is-every string=
    ("0"       (format NIL "~/print-byte-count/" 0))

    ("1kB"     (format NIL "~/print-byte-count/" 1000))
    ("1kB"     (format NIL "~@/print-byte-count/" 1000))
    ("1000"    (format NIL "~:/print-byte-count/" 1000))
    ("1000"    (format NIL "~:@/print-byte-count/" 1000))

    ("1kB"     (format NIL "~/print-byte-count/" 1024))
    ("1kB"     (format NIL "~:/print-byte-count/" 1024))
    ("1kB"     (format NIL "~@/print-byte-count/" 1024))
    ("1kB"     (format NIL "~:@/print-byte-count/" 1024))

    ("1KiB"    (format NIL "~,'T/print-byte-count/" 1000))
    ("1kB"     (format NIL "~,'T@/print-byte-count/" 1000))
    ("1000"    (format NIL "~,'T:/print-byte-count/" 1000))
    ("1000"    (format NIL "~,'T:@/print-byte-count/" 1000))

    ("1KiB"    (format NIL "~,'T/print-byte-count/" 1024))
    ("1KiB"    (format NIL "~,'T:/print-byte-count/" 1024))
    ("1kB"     (format NIL "~,'T@/print-byte-count/" 1024))
    ("1kB"     (format NIL "~,'T:@/print-byte-count/" 1024))

    ("1MB"     (format NIL "~/print-byte-count/" (expt 1024 2)))
    ("1GB"     (format NIL "~/print-byte-count/" (expt 1024 3)))
    ("1TB"     (format NIL "~/print-byte-count/" (expt 1024 4)))

    ("1.00kB"  (format NIL "~2/print-byte-count/" 1024))
    ("1.00KiB" (format NIL "~2,'T/print-byte-count/" 1024))
    ("0.98kB"  (format NIL "~2/print-byte-count/" 1000))
    ("0.98KiB" (format NIL "~2,'T/print-byte-count/" 1000))))

;;; example

(defun ls (pathname &optional (stream T))
  (dolist (filename (directory pathname))
    (let ((length (ignore-errors
                    (with-open-file (stream filename :element-type '(unsigned-byte 8))
                      (file-length stream)))))
      (format stream "~@[~/print-byte-count/~] ~A~%" length filename))))
