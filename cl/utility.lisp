(defpackage :gtirb/utility
  (:use :common-lisp
        :gtirb/version)
  (:import-from :cl-intbytes
                :int->octets
                :octets->int)
  (:import-from :alexandria
                :define-constant
                :read-stream-content-into-byte-vector
                :read-file-into-byte-vector
                :starts-with-subseq)
  (:import-from :uiop :nest)
  (:import-from :uiop/stream :file-stream-p)
  (:export :read-proto
           :write-proto
           :gtirb-magic-error
           :new-uuid
           :force-byte-array
           :uuid-to-integer
           :integer-to-uuid))
(in-package :gtirb/utility)
(declaim (optimize (speed 3) (safety 0) (debug 0)))

(define-constant gtirb-magic-octets #(71 84 73 82 66) ;; "GTIRB"
  :test #'equalp
  :documentation "GTIRB file magic bytes at beginning of magic header.")

(define-constant gtirb-magic-length 8
  :test #'equal
  :documentation "Number of bytes in the GTIRB file magic header.")

(define-condition gtirb-magic-error (error)
  ((message :initarg :message :initform nil :reader message))
  (:report (lambda (condition stream)
             (format stream "~S" (message condition))))
  (:documentation "Condition raised if GTIRB header is invalid."))

(defgeneric read-proto (class source)
  (:documentation "Read protobuf object of class CLASS from SOURCE.")
  (:method :before
    (class (path pathname))
    (declare (ignorable class))
    (assert (probe-file path) (path)
            "Can't read Protobuf from ~s, because the file doesn't exist."
            path))
  (:method (class (path string)) (read-proto class (pathname path)))
  (:method (class (path pathname))
    (with-open-file (input path :direction :input :element-type 'unsigned-byte)
      (read-proto class input)))
  (:method (class (input stream))
    (read-proto class (if (file-stream-p input)
                          (read-file-into-byte-vector input)
                          (read-stream-content-into-byte-vector input))))
  (:method (class (buffer array) &aux (gtirb (make-instance class)))
    (check-magic-header buffer)
    (pb:merge-from-array gtirb buffer gtirb-magic-length (length buffer))
    gtirb))

(defun write-proto (object path)
  "Write OBJECT to PATH."
  (let* ((size (pb:octet-size object))
         (buffer (make-array size :element-type '(unsigned-byte 8))))
    (pb:serialize object buffer 0 size)
    (with-open-file (output path
                            :direction :output :if-exists :supersede
                            :element-type 'unsigned-byte)
      (write-magic-header output)
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

(defun check-magic-header (bytes)
  "Check if the GTIRB magic header bytes are present in at the start of BYTES,
throwing a GTIRB-MAGIC-ERROR if the header is not present."
  (when (or (< (length bytes) gtirb-magic-length)
            (not (starts-with-subseq gtirb-magic-octets bytes)))
    (error (make-condition 'gtirb-magic-error
                           :message "File missing GTIRB magic - not a GTIRB file?")))
  (when (not (equal protobuf-version (aref bytes (1- gtirb-magic-length))))
    (error (nest (make-condition 'gtirb-magic-error :message)
                 (format nil "Attempt to decode IR of version ~d (expected version ~d)"
                         (aref bytes (1- gtirb-magic-length))
                         protobuf-version)))))

(defun write-magic-header (stream)
  "Write the GTIRB magic header bytes to STREAM."
  (write-sequence gtirb-magic-octets stream)
  (write-byte 0 stream)
  (write-byte 0 stream)
  (write-byte protobuf-version stream))
