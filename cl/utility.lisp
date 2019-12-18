(defpackage :gtirb/utility
  (:use :common-lisp)
  (:export :read-proto
           :write-proto
           :new-uuid
           :force-byte-array
           :uuid-to-integer
           :integer-to-uuid))
(in-package :gtirb/utility)
(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun read-proto (class path)
  "Read protobuf object of class CLASS from PATH."
  (assert (probe-file path) (path)
          "Can't read GTIRB from ~s, because the file doesn't exist."
          path)
  (let ((gtirb (make-instance class)))
    (with-open-file (input path
                           :direction :input :element-type 'unsigned-byte)
      (let* ((size (file-length input))
             (buffer (make-array size :element-type '(unsigned-byte 8))))
        (read-sequence buffer input)
        (pb:merge-from-array gtirb buffer 0 size)))
    gtirb))

(defun write-proto (object path)
  "Write OBJECT to PATH."
  (let* ((size (pb:octet-size object))
         (buffer (make-array size :element-type '(unsigned-byte 8))))
    (pb:serialize object buffer 0 size)
    (with-open-file (output path
                            :direction :output :if-exists :supersede
                            :element-type 'unsigned-byte)
      (write-sequence buffer output)))
  (values))

(defun new-uuid (&aux (it (make-array 16 :element-type '(unsigned-byte 8))))
  "Return a new random UUID."
  (dotimes (n 16 it) (setf (aref it n) (random 256))))

(defun force-byte-array (array)
  "Force ARRAY into a byte array."
  (declare (type (simple-array) array))
  (make-array (length array) :element-type '(unsigned-byte 8)
              :initial-contents array))

(defun uuid-to-integer (uuid)
  (if (emptyp uuid)
      ;;TODO: Needed when referent-uuid of a symbol can be #().
      ;;      Remove this case and instead stop processing missing
      ;;      referent-uuids.
      0
      (octets->int (force-byte-array uuid) 16)))

(defun integer-to-uuid (number)
  (int->octets number 16))
