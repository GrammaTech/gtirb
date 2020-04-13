;;;; data-symbols.lisp --- Print the names of all symbols pointing to data
;;;
;;; To run this example, do the following.
;;;
;;; 1. Install the gtirb and gtirb/dot packages with quicklisp
;;;
;;;    (mapcar #'ql:quickload '(:gt :gtirb)
;;;
;;; 2. Run ddisasm to disassemble a binary to GTIRB.
;;;
;;;    $ echo 'main(){puts("hello world");}'|gcc -x c - -o /tmp/hello
;;;    $ ddisasm /tmp/hello --ir /tmp/hello.gtirb
;;;
;;; 3. Evaluate this file using the path of the GTIRB file produced by
;;;    ddisasm and the addresses of two basic blocks.
;;;
(defpackage :data-symbols
  (:use :gt :gtirb)
  (:shadow :symbol :size))
(in-package :data-symbols)
(in-readtable :curry-compose-reader-macros)

(defun data-symbols (path)
  "Print the names of all symbols pointing to data in GTIRB at PATH."
  (nest (format t "~{~a~%~}")
        (mapcar #'name)
        (remove-if-not [{typep _ 'data-block} #'payload])
        (mappend #'symbols (modules (read-gtirb path)))))
