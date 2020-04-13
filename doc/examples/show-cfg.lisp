;;;; show-cfg.lisp --- An example program which opens an IR and draws the CFG
;;;
;;; To run this example, do the following.
;;;
;;; 1. Install the gtirb and gtirb/dot packages with quicklisp
;;;
;;;    (ql:quickload :gtirb/dot)
;;;
;;; 2. Run ddisasm to disassemble a binary to GTIRB.
;;;
;;;    $ echo 'main(){puts("hello world");}'|gcc -x c - -o /tmp/hello
;;;    $ ddisasm /tmp/hello --ir /tmp/hello.gtirb
;;;
;;; 3. Evaluate this file.
;;;
;;; 4. Process the resultant dot file into a png
;;;
;;;    $ cat /tmp/hello.dot|dot -Tpng > /tmp/hello.png
;;;
(in-package :gtirb/dot)
(to-dot-file (read-gtirb "/tmp/hello.gtirb") "/tmp/hello.dot")
