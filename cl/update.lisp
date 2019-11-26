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
       :documentation "display help output"))))

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

(defun byte-intervals (module)
  (error "TODO: byte-intervals from ~a" module))

(defgeneric upgrade (object &key &allow-other-keys)
  (:documentation "Upgrade OBJECT to the next protobuf version.")
  (:method ((old t) &key &allow-other-keys) old)
  (:method ((old array) &key  &allow-other-keys) (map 'vector #'upgrade old))
  (:method ((old proto-v0::ir) &key &allow-other-keys
            &aux (new (make-instance 'proto::ir)))
    (setf (proto::uuid new) (proto-v0::uuid old)
          (proto::version new) "1.0.0"
          (proto::aux-data new) (upgrade (proto-v0::aux-data-container old))
          (proto::modules new) (upgrade (proto-v0::modules old))))
  (:method ((old proto-v0::module) &key &allow-other-keys
            &aux (new (make-instance 'proto::module)))
    (mapc (lambda (field)
            (setf (slot-value new (intern field 'proto-v0))
                  (upgrade (funcall (intern field 'proto) old))))
          '(uuid
            binary-path
            preferred-addr
            rebase-delta
            file-format
            isa-id
            name
            symbols
            cfg
            proxies
            name
            aux-data))
    (setf (proto::sections new) (map 'vector {upgrade _ :module old}
                                     (proto-v0::sections old))))
  (:method ((old proto-v0::aux-data-container) &key  &allow-other-keys)
    (proto-v0::aux-data old))
  (:method ((old proto-v0::section) &key module &allow-other-keys
            &aux (new (make-instance 'proto::section)))
    (setf (proto::uuid new) (proto-v0::uuid old)
          (proto::name new) (proto-v0::name old)
          (proto::byte-intervals new) (byte-intervals module))))

(define-command update (input-file output-file &spec +udpate-args+)
  "Update GTIRB protobuf from INPUT-FILE to OUTPUT-FILE." ""
  (when help (show-help-for-update) (sb-ext:quit))
  (write-proto (upgrade (read-proto 'proto-v0:ir input-file)) output-file))
