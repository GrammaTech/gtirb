;;;; cfg-paths.lisp --- Print the shortest path between two blocks
;;;
;;; To run this example, do the following.
;;;
;;; 1. Install all required packages with quicklisp.
;;;
;;;    NOTE: you might need to install the :graph library from source
;;;          as the `all-paths' function isn't in the quicklisp
;;;          version at the time  of writing of this example.
;;;
;;;    (mapcar #'ql:quickload '(:gt :gtirb :graph)
;;;
;;; 2. Run ddisasm to disassemble a binary to GTIRB.
;;;
;;;    $ echo 'main(){puts("hello world");}'|gcc -x c - -o /tmp/hello
;;;    $ ddisasm /tmp/hello --ir /tmp/hello.gtirb
;;;
;;; 3. Evaluate this file using the path of the GTIRB file produced by
;;;    ddisasm and the addresses of two basic blocks.
;;;
(defpackage :cfg-paths
  (:use :gt :gtirb :graph)
  (:shadow :symbol :size :copy))
(in-package :cfg-paths)

(defun cfg-paths (path from to)
  "Return all paths in the cfg of the GTIRB file from FROM to TO.
FROM and TO are the addresses of the start of blocks."
  (let ((ir (read-gtirb path)))
    (flet ((block-uuid-at (address)
             (uuid (find-if (lambda (it) (typep it 'code-block))
                            (at-address ir address)))))
      (all-paths (cfg ir) (block-uuid-at from) (block-uuid-at to)))))
