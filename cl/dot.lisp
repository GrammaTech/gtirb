(defpackage :gtirb/dot
  (:use :common-lisp :alexandria :graph :graph/dot :gtirb
        :named-readtables :curry-compose-reader-macros)
  (:import-from :uiop :nest)
  (:export :to-dot :to-dot-file))
(use-package :gtirb/dot)
(in-readtable :curry-compose-reader-macros)

(defun dot-edge-label (graph edge)
  (let ((obj (car (edge-value graph edge))))
    (format nil "\"~a[~a:~a]\""
            (edge-type obj)
            (if (conditional obj) :c :u)
            (if (direct obj) :d :i))))

(defmethod to-dot ((obj module) &rest rest)
  "Write the CFG of MODULE to the Graphviz graphing language."
  (apply #'to-dot (cfg obj)
         :edge-attrs (list (cons :label {dot-edge-label (cfg obj)}))
         rest))
