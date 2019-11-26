(defpackage :gtirb/update
  (:use :common-lisp
        :alexandria
        :named-readtables
        :curry-compose-reader-macros
        :command-line-arguments)
  (:export :update))
(in-package :gtirb/update)
(in-readtable :curry-compose-reader-macros)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +udpate-args+
    '((("help" #\h #\?) :type boolean :optional t
       :documentation "display help output")
      (("downgrade" #\d) :type boolean :optional t
       :documentation "downgrade GTIRB protobuf version instead of upgrade"))))

(defun read-proto (version path)
  "Read GTIRB protobuf version VERSION from PATH."
  (assert (probe-file path) (path)
          "Can't read GTIRB from ~s, because the file doesn't exist."
          path)
  (let ((gtirb (make-instance version)))
    (with-open-file (input path
                           :direction :input :element-type 'unsigned-byte)
      (let* ((size (file-length input))
             (buffer (make-array size :element-type '(unsigned-byte 8))))
        (read-sequence buffer input)
        (pb:merge-from-array gtirb buffer 0 size)))
    gtirb))

(defun write-proto (gtirb path)
  "Write GTIRB to PATH."
  (let* ((size (pb:octet-size gtirb))
         (buffer (make-array size :element-type '(unsigned-byte 8))))
    (pb:serialize gtirb buffer 0 size)
    (with-open-file (output path
                            :direction :output :if-exists :supersede
                            :element-type 'unsigned-byte)
      (write-sequence buffer output)))
  (values))

(defun v0-to-v1 (proto)
  proto)

(defun v1-to-v0 (proto)
  proto)

(define-command update (input-file output-file &spec +udpate-args+)
  "Update GTIRB protobuf from INPUT-FILE to OUTPUT-FILE."
  ""
  (when help (show-help-for-update) (sb-ext:quit))
  (let ((input-version (if downgrade 'proto-v0:ir 'proto:ir)))
    (write-proto (funcall (if downgrade #'v1-to-v0 #'v0-to-v1)
                          (read-proto input-version input-file)) output-file)))
