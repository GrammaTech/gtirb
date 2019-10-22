(defpackage :gtirb/gtirb
  (:nicknames :gtirb)
  (:use :common-lisp)
  (:export :read-gtirb))
(in-package :gtirb/gtirb)

(defun read-gtirb (path)
  "Read GTIRB from PATH."
  (assert (probe-file path) (path)
          "Can't read GTIRB from ~s, because the file doesn't exist."
          path)
  (let ((gtirb (make-instance 'proto:ir)))
    (with-open-file (input path
                           :direction :input :element-type 'unsigned-byte)
      (let* ((size (file-length input))
             (buffer (make-array size :element-type '(unsigned-byte 8))))
        (read-sequence buffer input)
        (pb:merge-from-array gtirb buffer 0 size)))
    gtirb))
