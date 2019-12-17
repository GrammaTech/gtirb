(defpackage :gtirb/dot
  (:use :common-lisp :alexandria :graph :graph/dot :gtirb
        :command-line-arguments
        :named-readtables :curry-compose-reader-macros)
  (:import-from :uiop :nest)
  (:import-from :proto-v0 :ir)
  (:shadowing-import-from :gtirb :symbol)
  (:export :to-dot :to-dot-file))
(in-package :gtirb/dot)
(in-readtable :curry-compose-reader-macros)

(defun dot-edge-label (graph edge)
  (let ((obj (car (edge-value graph edge))))
    (format nil "\"~a[~a:~a]\""
            (edge-type obj)
            (if (conditional obj) :c :u)
            (if (direct obj) :d :i))))

(defmethod to-dot ((obj gtirb) &rest rest)
  "Write the CFG of MODULE to the Graphviz graphing language."
  (apply #'to-dot (cfg obj)
         :edge-attrs (list (cons :label {dot-edge-label (cfg obj)}))
         rest))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +udpate-args+
    '((("help" #\h #\?) :type boolean :optional t
       :documentation "display help output"))))

(define-command dot (gtirb-file dot-file &spec +udpate-args+)
  "Write first GTIRB module in GTIRB-FILE to DOT-FILE." ""
  (when help (show-help-for-dot) (sb-ext:quit))
  (to-dot-file (read-gtirb gtirb-file) dot-file))
