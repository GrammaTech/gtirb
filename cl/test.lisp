(defpackage :gtirb/test
  (:use :common-lisp :gtirb :stefil)
  (:export :gtirb))
(in-package :gtirb/test)

(defsuite gtirb)

(deftest simple-read ()
  (is t))
