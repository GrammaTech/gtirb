(defpackage :gtirb/gtirb
  (:nicknames :gtirb)
  (:use :common-lisp)
  (:shadow :symbol :block)
  (:export :read-gtirb-proto :write-gtirb-proto))
(in-package :gtirb/gtirb)

(defun read-gtirb-proto (path)
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

(defun write-gtirb-proto (gtirb path)
  "Write GTIRB to PATH."
  (let* ((size (pb:octet-size gtirb))
         (buffer (make-array size :element-type '(unsigned-byte 8))))
    (pb:serialize gtirb buffer 0 size)
    (with-open-file (output path
                            :direction :output :if-exists :supersede
                            :element-type 'unsigned-byte)
      (write-sequence buffer output)))
  (values))

;;; TODO: Allow setf through this function.
(defun decode-file-format (file-format-number)
  (declare (type fixnum file-format-number)
           (optimize (speed 3)))
  (ecase file-format-number
    (#.proto:+file-format-coff+ :coff)
    (#.proto:+file-format-elf+ :elf)
    (#.proto:+file-format-ida-pro-db32+ :ida-pro-db32)
    (#.proto:+file-format-ida-pro-db64+ :ida-pro-db64)
    (#.proto:+file-format-macho+ :macho)
    (#.proto:+file-format-pe+ :pe)
    (#.proto:+file-format-raw+ :raw)
    (#.proto:+file-format-xcoff+ :xcoff)
    (#.proto:+file-format-format-undefined+ :format-undefined)))

;;; TODO: Allow setf through this function.
(defun decode-isa (isa-number)
  (declare (type fixnum isa-number)
           (optimize (speed 3)))
  (ecase isa-number
    (#.proto:+isaid-isa-undefined+ :undefined)
    (#.proto:+isaid-ia32+ :ia32)
    (#.proto:+isaid-ppc32+ :ppc32)
    (#.proto:+isaid-x64+ :x64)
    (#.proto:+isaid-arm+ :arm)
    (#.proto:+isaid-valid-but-unsupported+ :valid-but-unsupported)))


;;;; Class
;;;
;;; Started fleshing out classes to wrap the raw protobuf.  One
;;; important decision is how much we want to wrap everything.  E.g.,
;;; should we simply hold pointers to the protobuf, and parse/update
;;; that on every access?  Alternately, should we be converting
;;; everything from protobuf to more idiomatic Common Lisp objects
;;; actually stored on the classes (as done with modules of gtirb
;;; objects below)?
;;;
;;; We probably want a wrapper for the graph at least, I'm not sure
;;; about the other elements of the GTIRB or the module.
;;;
(defclass section ()
  ((proto :initarg :proto :type proto:section
          :documentation "Backing protobuf object.")))

(defclass symbol ()
  ((proto :initarg :proto :type proto:symbol
          :documentation "Backing protobuf object.")))

(defclass symbolic-operand ()
  ((proto :initarg :proto :type proto::module-symbolic-operands-entry
          :documentation "Backing protobuf object.")))

(defclass cfg ()
  ((proto :initarg :proto :type proto:cfg
          :documentation "Backing protobuf object.")))

(defclass block ()
  ((proto :initarg :proto :type proto:block
          :documentation "Backing protobuf object.")))

(defclass data ()
  ((proto :initarg :proto :type proto:data
          :documentation "Backing protobuf object.")))

(defclass proxy ()
  ((proto :initarg :proto :type proto::proxy-block
          :documentation "Backing protobuf object.")))

(defclass aux-data ()
  ((proto :initarg :proto :type proto:aux-data
          :documentation "Backing protobuf object.")))

(defclass module ()
  ((proto :initarg :proto :accessor proto :type proto:module
          :documentation "Backing protobuf object.")
   (file-format :reader raw-file-format :writer set-raw-file-format
                :documentation "Instruction Set Architecture (FILE-FORMAT)")
   (sections :accessor sections :type (list section)
             :documentation "Module sections.")
   (symbols :accessor symbols :type (list symbol)
            :documentation "Module symbols.")
   (symbolic-operands :accessor symbolic-operands :type (list symbolic-operand)
                      :documentation "Module symbolic-operands.")
   (cfgs :accessor cfgs :type cfg
         :documentation "Module control flow block (CFG).")
   (blocks :accessor blocks :type (list block)
           :documentation "Module code blocks.")
   (data :accessor data :type (list data)
         :documentation "Module data objects.")
   (proxies :accessor proxies :type (list proxy)
            :documentation "Module proxies.")
   (aux-data :accessor aux-data :type (list aux-data)
             :documentation "Module auxiliary data objects.")))

(defmethod name ((obj module))
  (pb:string-value (proto:name (proto obj))))

(defmethod isa ((obj module))
  (decode-isa (proto:isa-id (proto obj))))

(defmethod file-format ((obj module))
  (decode-file-format (proto:file-format (proto obj))))

(defmethod print-object ((obj module) (stream stream))
  (print-unreadable-object (obj stream :type t :identity cl:t)
    (format stream "~a:~a:~s" (file-format obj) (isa obj) (name obj))))

(defclass gtirb ()
  ((proto :initarg :proto :accessor proto :type proto:module
          :documentation "Backing protobuf object.")
   (modules :initarg modules :accessor modules :initform nil :type (list module))))

(defmethod initialize-instance :after ((obj gtirb) &key)
  (with-slots (modules) obj
    (setf modules (mapcar (lambda (module-proto)
                            (make-instance 'module :proto module-proto))
                          (coerce (proto:modules (proto obj)) 'list)))))

(defmethod print-object ((obj gtirb) (stream stream))
  (print-unreadable-object (obj stream :type t :identity cl:t)
    (format stream "~a" (modules obj))))
