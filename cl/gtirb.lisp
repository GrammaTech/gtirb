(defpackage :gtirb/gtirb
  (:nicknames :gtirb)
  (:use :common-lisp :alexandria :graph :trivia
        :trivial-utf-8
        :named-readtables :curry-compose-reader-macros)
  (:import-from :uiop :nest)
  (:import-from :cl-intbytes
                :octets->int64 :int64->octets
                :octets->uint64)
  (:export :read-gtirb-proto :write-gtirb-proto
           :module
           :name
           :isa
           :file-format
           :get-block
           :edge-type
           :conditional
           :direct
           :aux-data
           :aux-data-type
           :data
           :blocks
           :modules
           :cfg
           :gtirb))
(in-package :gtirb/gtirb)
(in-readtable :curry-compose-reader-macros)

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
(defclass module ()
  ((proto :initarg :proto :accessor proto :type proto:module
          :documentation "Backing protobuf object.")
   (cfg :accessor cfg :type cfg
        :documentation "Module control flow block (CFG).")
   (blocks :accessor blocks :type hash-table
           :documentation "Module control flow block (CFG).")
   (aux-data :accessor aux-data :type (list aux-data)
             :documentation "Module auxiliary data objects.")))

(defmethod name ((obj module))
  (pb:string-value (proto:name (proto obj))))

(defmethod (setf name) ((obj module) new)
  (setf (proto:name (proto obj)) (pb:string-field new)))

(define-constant +module-isa-map+
    '((#.proto:+isaid-isa-undefined+ . :undefined)
      (#.proto:+isaid-ia32+ . :ia32)
      (#.proto:+isaid-ppc32+ . :ppc32)
      (#.proto:+isaid-x64+ . :x64)
      (#.proto:+isaid-arm+ . :arm)
      (#.proto:+isaid-valid-but-unsupported+ . :valid-but-unsupported))
  :test #'equal)

(defmethod isa ((obj module))
  (cdr (assoc (proto:isa-id (proto obj)) +module-isa-map+)))

(defmethod (setf isa) (new (obj module))
  (setf (proto:isa-id (proto obj))
        (car (rassoc new +module-isa-map+))))

(define-constant +module-file-format-map+
    '((#.proto:+file-format-coff+ . :coff)
      (#.proto:+file-format-elf+ . :elf)
      (#.proto:+file-format-ida-pro-db32+ . :ida-pro-db32)
      (#.proto:+file-format-ida-pro-db64+ . :ida-pro-db64)
      (#.proto:+file-format-macho+ . :macho)
      (#.proto:+file-format-pe+ . :pe)
      (#.proto:+file-format-raw+ . :raw)
      (#.proto:+file-format-xcoff+ . :xcoff)
      (#.proto:+file-format-format-undefined+ . :format-undefined))
  :test #'equal)

(defmethod file-format ((obj module))
  (cdr (assoc (proto:file-format (proto obj)) +module-file-format-map+)))

(defmethod (setf file-format) (new (obj module))
  (setf (proto:file-format (proto obj))
        (car (rassoc new +module-file-format-map+))))

(defmethod initialize-instance :after ((obj module) &key)
  ;; Repackage the AuxData into an alist keyed by name.
  (let ((p-aux-data (proto:aux-data (proto:aux-data-container (proto obj))))
        (aux-data '()))
    (dotimes (n (length p-aux-data))
      (push (cons (pb:string-value (proto:key (aref p-aux-data n)))
                  (make-instance 'aux-data
                    :proto (proto:value (aref p-aux-data n))))
            aux-data))
    (setf (aux-data obj) aux-data))
  ;; Package the blocks into a has keyed by UUID.
  (let ((p-blocks (proto:blocks (proto obj)))
        (block-h (make-hash-table)))
    (dotimes (n (length p-blocks))
      (let ((p-block (aref p-blocks n)))
        (setf (gethash (uuid-to-integer (proto:uuid p-block))
                       block-h)
              p-block)))
    (setf (blocks obj) block-h))
  ;; Build the CFG as a lisp graph.
  (nest
   (with-slots (cfg) obj)
   (let ((p-cfg (proto:cfg (proto obj)))))
   (setf cfg)
   (populate
    (make-instance 'digraph)
    :edges-w-values
    (mapcar (lambda (edge)
              (list (list (uuid-to-integer (proto:source-uuid edge))
                          (uuid-to-integer (proto:target-uuid edge)))
                    (make-instance 'edge-label :proto (proto:label edge))))
            (coerce (proto:edges p-cfg) 'list))
    :nodes (map 'list  #'uuid-to-integer (proto:vertices p-cfg)))))

(defmethod get-block ((uuid simple-array) (obj module))
  (get-block (uuid-to-integer uuid) obj))

(defmethod (setf get-block) (new (uuid simple-array) (obj module))
  (setf (get-block (uuid-to-integer uuid) obj) new))

(defmethod get-block ((uuid integer) (obj module))
  (gethash uuid (blocks obj)))

(defmethod (setf get-block) (new (uuid integer) (obj module))
  (setf (gethash uuid (blocks obj)) new))

(defun uuid-to-integer (uuid)
  (octets->int64
   (make-array 16 :element-type '(unsigned-byte 8) :initial-contents uuid)))

(defun integer-to-uuid (number)
  (int64->octets number))

(defmethod print-object ((obj module) (stream stream))
  (print-unreadable-object (obj stream :type t :identity cl:t)
    (format stream "~a ~a ~s" (file-format obj) (isa obj) (name obj))))

(defclass edge-label ()
  ((proto :initarg :proto :accessor proto :type proto:module
          :documentation "Backing protobuf object.")))

(define-constant +edge-label-type-map+
    '((#.proto:+edge-type-type-branch+ . :branch)
      (#.proto:+edge-type-type-call+ . :call)
      (#.proto:+edge-type-type-fallthrough+ . :fallthrough)
      (#.proto:+edge-type-type-return+ . :return)
      (#.proto:+edge-type-type-syscall+ . :syscall)
      (#.proto:+edge-type-type-sysret+ . :sysret))
  :test #'equal)

(defmethod edge-type ((obj edge-label))
  (cdr (assoc (proto:type (proto obj)) +edge-label-type-map+)))

(defmethod (setf edge-type) (new (obj edge-label))
  (setf (proto:type (proto obj))
        (car (rassoc new +edge-label-type-map+))))

(defmethod conditional ((obj edge-label))
  (proto:conditional (proto obj)))

(defmethod (setf conditional) (new (obj edge-label))
  (setf (proto:conditional (proto obj)) new))

(defmethod direct ((obj edge-label))
  (proto:direct (proto obj)))

(defmethod (setf direct) (new (obj edge-label))
  (setf (proto:direct (proto obj)) new))

(defmethod print-object ((obj edge-label) (stream stream))
  (print-unreadable-object (obj stream :type t :identity cl:t)
    (format stream "~a ~a ~a"
            (edge-type obj)
            (if (conditional obj) :conditional :unconditional)
            (if (direct obj) :direct :undirect))))

(defclass aux-data ()
  ((proto :initarg :proto :accessor proto :type proto:module
          :documentation "Backing protobuf object.")))

(defmacro start-case (string &body body)
  `(progn
     ;; (declare (type string ,string))
     (assert (stringp ,string) (,string) "Argument ~s is not a string." ,string)
     (cond
       ,@(mapcar (lambda (form)
                   (destructuring-bind (prefix . body) form
                     (if (stringp prefix)
                         `((eql (search ,prefix ,string) 0)
                           (let ((,string (subseq ,string ,(length prefix))))
                             ,@body))
                         (cons prefix body))))
                 body))))

(defun matching (open-char close-char string)
  (let ((offset 1))
    (dotimes (n (length string))
      (cond
        ((eql (aref string n) open-char) (incf offset))
        ((eql (aref string n) close-char) (decf offset)))
      (when (zerop offset)
        (return-from matching n))))
  (error "Can't close (~a ~a) in ~s." open-char close-char string))

(defun aux-data-type-read (type-string)
  (when (and type-string (not (emptyp type-string)))
    (start-case type-string
      ("mapping<"
       (let ((close (matching #\< #\> type-string)))
         (cons (cons :mapping (aux-data-type-read (subseq type-string 0 close)))
               (aux-data-type-read (subseq type-string close)))))
      ("set<"
       (let ((close (matching #\< #\> type-string)))
         (cons (cons :set (aux-data-type-read (subseq type-string 0 close)))
               (aux-data-type-read (subseq type-string close)))))
      ("sequence<"
       (let ((close (matching #\< #\> type-string)))
         (cons (cons :sequence (aux-data-type-read (subseq type-string 0 close)))
               (aux-data-type-read (subseq type-string close)))))
      ("tuple<"
       (let ((close (matching #\< #\> type-string)))
         (cons (cons :tuple (aux-data-type-read (subseq type-string 0 close)))
               (aux-data-type-read (subseq type-string close)))))
      ("," (aux-data-type-read type-string))
      (">" (aux-data-type-read type-string))
      ("UUID" (cons :uuid (aux-data-type-read type-string)))
      ("Addr" (cons :addr (aux-data-type-read type-string)))
      ("Offset" (cons :offset (aux-data-type-read type-string)))
      ("string" (cons :string (aux-data-type-read type-string)))
      ("uint64_t" (cons :uint64-t (aux-data-type-read type-string)))
      ("int64_t" (cons :int64-t (aux-data-type-read type-string)))
      (t (error "Junk in type string ~a" type-string)))))

(defmethod aux-data-type ((obj aux-data))
  (first (aux-data-type-read (pb:string-value (proto:type-name (proto obj))))))

(defun aux-data-type-print (aux-data-type)
  (when aux-data-type
    (if (listp aux-data-type)
        (case (first aux-data-type)
          (:mapping
           (concatenate 'string
                        "mapping<" (aux-data-type-print (second aux-data-type))
                        "," (aux-data-type-print (third aux-data-type)) ">"))
          (:set
           (concatenate 'string
                        "set<" (aux-data-type-print (second aux-data-type)) ">"))
          (:sequence
           (concatenate 'string
                        "sequence<" (aux-data-type-print (second aux-data-type)) ">"))
          (:tuple
           (concatenate 'string
                        "tuple<"
                        (aux-data-type-print (second aux-data-type))
                        (apply #'concatenate 'string
                               (mapcar [{concatenate 'string ","} #'aux-data-type-print]
                                       (cddr aux-data-type)))
                        ">")))
        (case aux-data-type
          (:uuid "UUID")
          (:addr "Addr")
          (:offset "Offset")
          (:string "string")
          (:uint64-t "uint64_t")
          (:int64-t "int64_t")))))

(defmethod (setf aux-data-type) ((new string) (obj aux-data))
  (setf (proto:type-name (proto obj)) (pb:string-field new)))

(defmethod data ((obj aux-data))
  ;; TODO: Implement the parsing and reading/writing of data by type.
  (warn "Not implemented for ~a." obj))

(defmethod (setf data) (new (obj aux-data))
  ;; TODO: Implement the parsing and reading/writing of data by type.
  (warn "Not implemented for ~a." obj))

(defvar *decode-data* nil)
(defun advance (n) (setf *decode-data* (subseq *decode-data* n)))
(defun decode (type)
  ;; TODO: Something doesn't appear to be working here.
  (values
   (match type
     ((or :addr :uint64-t)
      (prog1
          (octets->uint64 (subseq *decode-data* 0 8))
        (advance 8)))
     (:int64-t
      (prog1
          (octets->int64 (subseq *decode-data* 0 8))
        (advance 8)))
     (:uuid
      (prog1 (subseq *decode-data* 0 8) (advance 8)))
     (:offset
      (list (decode :uuid) (decode :uint64-t)))
     (:string
      (let ((size (decode :uint64-t)))
        (prog1 (utf-8-bytes-to-string (subseq *decode-data* 0 size))
          (advance size))))
     ((list :mapping key-t value-t)
      (let ((result (make-hash-table)))
        (dotimes (n (decode :uint64-t) result)
          (declare (ignorable n))
          (let* ((key (decode key-t))
                 (value (decode value-t)))
            (setf (gethash key result) value)))))
     ((list (or :sequence :set) type)
      (let (result)
        (reverse
         (dotimes (n (decode :uint64-t) result)
           (declare (ignorable n))
           (push (decode type) result)))))
     ((list* :tuple types)
      (mapcar #'decode types)))
   (length *decode-data*)))
(defun aux-data-decode (type data)
  (let ((*decode-data* data))
    (decode type)))

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

(defgeneric to-proto (object)
  (:documentation
   "Dump an updated protocol buffer for OBJECT.
This will ensure that any changes made to objects outside of the
protocol buffer object, e.g. blocks on modules, are synchronized
against the protocol buffer object before it is returned.  We could
incrementally synchronize everything to the backing protocol buffer,
but that would likely get expensive.")
  ;; TODO: Implement this for the above classes.
  ;;
  ;; Module needs the following to be updated:
  ;; - aux-data
  ;; - blocks
  ;; - CFG
  (:method (object)
    (warn "Not updating protocol buffer for ~a." object)))
