;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: cl-user; -*-

(in-package #:cl-user)

(asdf:defsystem #:cl-print-byte-count
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :description "Pretty print byte and other quantities."
  :long-description "Format byte quantities with their SI prefix, ready to use with FORMAT."
  :author "Olof-Joachim Frahm <olof@macrolet.net>"
  :licence "Public Domain"
  :components ((:doc-file "README")
               (:file "print-byte-count"))
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-print-byte-count-tests)))
  :perform (asdf:test-op :after (operation component)
                         (funcall (intern (symbol-name '#:run!) '#:fiveam)
                                  (intern (symbol-name '#:print-byte-count)
                                          '#:cl-print-byte-count-tests))))

(asdf:defsystem #:cl-print-byte-count-tests
  :author "Olof-Joachim Frahm <olof@macrolet.net>"
  :depends-on (#:cl-print-byte-count #:fiveam)
  :components ((:file "tests")))
