(defpackage :gtirb/version
  (:use :common-lisp)
  (:import-from :alexandria
                :define-constant)
  (:import-from :asdf/system :system-relative-pathname)
  (:import-from :uiop :nest)
  (:export :gtirb-version
           :protobuf-version))
(in-package :gtirb/version)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar version.txt
    `#.(nest (let ((version-path
                    (system-relative-pathname "gtirb" "../version.txt"))))
             (with-open-file (in version-path))
             (loop for line = (read-line in nil :eof)
                until (eql line :eof)
                collect (let ((delim (position #\Space line)))
                          (cons (intern (subseq line 0 delim))
                                (parse-integer (subseq line (1+ delim)))))))))

(define-constant gtirb-version
    (format nil "~d.~d.~d"
            (cdr (assoc 'VERSION_MAJOR version.txt))
            (cdr (assoc 'VERSION_MINOR version.txt))
            (cdr (assoc 'VERSION_PATCH version.txt)))
  :test #'string=
  :documentation "GTIRB Version as a string of \"MAJOR.MINOR.PATCH\".")

(define-constant protobuf-version
    (cdr (assoc 'VERSION_PROTOBUF version.txt))
  :test #'=
  :documentation "GTIRB Protobuf Version as a non-negative integer.")
