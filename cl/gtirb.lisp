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


;;;; Classes
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
(defclass aux-data ()
  ((proto :initarg :proto :type proto:aux-data
          :documentation "Backing protobuf object.")))

;;; Module
(defclass module ()
  ((proto :initarg :proto :accessor proto :type proto:module
          :documentation "Backing protobuf object.")
   (cfg :accessor cfgs :type cfg
        :documentation "Module control flow block (CFG).")
   (aux-data :accessor aux-data :type (list aux-data)
             :documentation "Module auxiliary data objects.")))

(defmethod name ((obj module))
  (pb:string-value (proto:name (proto obj))))

(defmethod (setf name) ((obj module) new)
  (setf (proto:name (proto obj)) (pb:string-field new)))

(defconstant +module-isa-map+
  '((#.proto:+isaid-isa-undefined+ . :undefined)
    (#.proto:+isaid-ia32+ . :ia32)
    (#.proto:+isaid-ppc32+ . :ppc32)
    (#.proto:+isaid-x64+ . :x64)
    (#.proto:+isaid-arm+ . :arm)
    (#.proto:+isaid-valid-but-unsupported+ . :valid-but-unsupported)))

(defmethod isa ((obj module))
  (cdr (assoc (proto:isa-id (proto obj)) +module-isa-map+)))

(defmethod (setf isa) (new (obj module))
  (setf (proto:isa-id (proto obj))
        (car (rassoc new +module-isa-map+))))

(defconstant +module-file-format-map+
  '((#.proto:+file-format-coff+ . :coff)
    (#.proto:+file-format-elf+ . :elf)
    (#.proto:+file-format-ida-pro-db32+ . :ida-pro-db32)
    (#.proto:+file-format-ida-pro-db64+ . :ida-pro-db64)
    (#.proto:+file-format-macho+ . :macho)
    (#.proto:+file-format-pe+ . :pe)
    (#.proto:+file-format-raw+ . :raw)
    (#.proto:+file-format-xcoff+ . :xcoff)
    (#.proto:+file-format-format-undefined+ . :format-undefined)))

(defmethod file-format ((obj module))
  (cdr (assoc (proto:file-format (proto obj)) +module-file-format-map+)))

(defmethod (setf file-format) (new (obj module))
  (setf (proto:file-format (proto obj))
        (car (rassoc new +module-file-format-map+))))

(defmethod blocks ((obj module))
  (proto:blocks (proto obj)))

(defmethod (setf blocks) (new (obj module))
  (setf (proto:blocks (proto obj)) new))

(defmethod print-object ((obj module) (stream stream))
  (print-unreadable-object (obj stream :type t :identity cl:t)
    (format stream "~a ~a ~s" (file-format obj) (isa obj) (name obj))))

;;; GTIRB
(defclass gtirb ()
  ((proto :initarg :proto :accessor proto :type proto:module
          :documentation "Backing protobuf object.")
   (modules :initarg modules :accessor modules :initform nil :type (list module)
            :documentation "List of the modules on an IR.")))

(defmethod (setf modules) :after (new (obj gtirb))
  (setf (proto:modules (proto obj))
        (coerce (mapcar #'proto (modules obj)) 'vector)))

(defmethod initialize-instance :after ((obj gtirb) &key)
  (with-slots (modules) obj
    (setf modules (mapcar (lambda (module-proto)
                            (make-instance 'module :proto module-proto))
                          (coerce (proto:modules (proto obj)) 'list)))))

(defmethod print-object ((obj gtirb) (stream stream))
  (print-unreadable-object (obj stream :type t :identity cl:t)
    (format stream "~a" (modules obj))))
