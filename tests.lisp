;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: cl-user; -*-

(in-package #:cl-user)

(defpackage #:cl-print-byte-count-tests
  (:use #:cl #:cl-print-byte-count #:fiveam))

(in-package #:cl-print-byte-count-tests)

(def-suite print-byte-count)

(in-suite print-byte-count)

(def-test test.1 ()
  (is-every string=
    ("0"       (format NIL "~/cl-print-byte-count:print-byte-count/" 0))

    ("1kB"     (format NIL "~/cl-print-byte-count:print-byte-count/" 1000))
    ("1kB"     (format NIL "~@/cl-print-byte-count:print-byte-count/" 1000))
    ("1000"    (format NIL "~:/cl-print-byte-count:print-byte-count/" 1000))
    ("1000"    (format NIL "~:@/cl-print-byte-count:print-byte-count/" 1000))

    ("1kB"     (format NIL "~/cl-print-byte-count:print-byte-count/" 1024))
    ("1kB"     (format NIL "~:/cl-print-byte-count:print-byte-count/" 1024))
    ("1kB"     (format NIL "~@/cl-print-byte-count:print-byte-count/" 1024))
    ("1kB"     (format NIL "~:@/cl-print-byte-count:print-byte-count/" 1024))

    ("1KiB"    (format NIL "~,'T/cl-print-byte-count:print-byte-count/" 1000))
    ("1kB"     (format NIL "~,'T@/cl-print-byte-count:print-byte-count/" 1000))
    ("1000"    (format NIL "~,'T:/cl-print-byte-count:print-byte-count/" 1000))
    ("1000"    (format NIL "~,'T:@/cl-print-byte-count:print-byte-count/" 1000))

    ("1KiB"    (format NIL "~,'T/cl-print-byte-count:print-byte-count/" 1024))
    ("1KiB"    (format NIL "~,'T:/cl-print-byte-count:print-byte-count/" 1024))
    ("1kB"     (format NIL "~,'T@/cl-print-byte-count:print-byte-count/" 1024))
    ("1kB"     (format NIL "~,'T:@/cl-print-byte-count:print-byte-count/" 1024))

    ("1MB"     (format NIL "~/cl-print-byte-count:print-byte-count/" (expt 1024 2)))
    ("1GB"     (format NIL "~/cl-print-byte-count:print-byte-count/" (expt 1024 3)))
    ("1TB"     (format NIL "~/cl-print-byte-count:print-byte-count/" (expt 1024 4)))

    ("1.00kB"  (format NIL "~2/cl-print-byte-count:print-byte-count/" 1024))
    ("1.00KiB" (format NIL "~2,'T/cl-print-byte-count:print-byte-count/" 1024))
    ("0.98kB"  (format NIL "~2/cl-print-byte-count:print-byte-count/" 1000))
    ("0.98KiB" (format NIL "~2,'T/cl-print-byte-count:print-byte-count/" 1000))))
