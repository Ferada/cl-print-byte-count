<!-- -*- mode: markdown; coding: utf-8; -*- -->

CL-PRINT-BYTE-COUNT - Pretty print byte and other quantities.

Copyright (C) 2012 Olof-Joachim Frahm

Release as Public Domain.

Uses fiveam for defining tests.

# USAGE

Best see the `tests.lisp` file for the different options.  Basically the
default is:  Use multiples of two to count bytes, but use multiples of
ten to determine the threshold, i.e. printing `1000` results in `1kB`,
whereas `999` gets no postfix.  Rounding is done via `ROUND`, but
supplying a number of digits will use monetary format.

The `KiB` convention isn't used, but may be enabled for base two.  Other
postfixes may be used as well by adding them to the `FORMAT` call.

# EXAMPLES

List a directory and the size of each file:

    (defun ls (pathname &optional (stream T))
      (dolist (filename (directory pathname))
        (let ((length (ignore-errors
                        (with-open-file (stream filename :element-type '(unsigned-byte 8))
                          (file-length stream)))))
          (format stream "~@[~/cl-print-byte-count:print-byte-count/~] ~A~%" length filename))))

    (ls ".")
    => 3kB /bin/
       48 /boot/
       13kB /dev/
       ...
