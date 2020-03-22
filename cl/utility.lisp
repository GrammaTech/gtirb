(defpackage :gtirb/utility
  (:use :common-lisp)
  (:import-from :protocol-buffer)
  (:import-from :cl-intbytes
                :int->octets
                :octets->int)
  (:import-from :alexandria
                :read-stream-content-into-byte-vector
                :read-file-into-byte-vector)
  (:export :read-proto
           :write-proto
           :new-uuid
           :force-byte-array
           :uuid-to-integer
           :integer-to-uuid))
(in-package :gtirb/utility)
(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defgeneric read-proto (class source)
  (:documentation "Read protobuf object of class CLASS from SOURCE.")
  (:method :before
    (class (path pathname))
    (assert (probe-file path) (path)
            "Can't read Protobuf from ~s, because the file doesn't exist."
            path))
  (:method (class (path string)) (read-proto class (pathname path)))
  (:method (class (path pathname))
    (with-open-file (input path :direction :input :element-type 'unsigned-byte)
      (read-proto class input)))
  (:method (class (input stream) &aux (gtirb (make-instance class)))
    (let ((buffer (if (uiop/stream::file-stream-p input)
                      (read-file-into-byte-vector input)
                      (read-stream-content-into-byte-vector input))))
      (pb:merge-from-array gtirb buffer 0 (length buffer)))
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
  (declare (type (simple-array) uuid))
  (if (zerop (length uuid))
      (prog1 0 #+debug (warn "Bad null UUID."))
      (octets->int (force-byte-array uuid) 16)))

(defun integer-to-uuid (number)
  (int->octets number 16))
