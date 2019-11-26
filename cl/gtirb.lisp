(defpackage :gtirb/gtirb
  (:nicknames :gtirb)
  (:use :common-lisp :alexandria :graph :trivia
        :trivial-utf-8
        :named-readtables :curry-compose-reader-macros)
  (:shadow :symbol :block)
  (:import-from :uiop :nest)
  (:import-from :cl-intbytes
                :int->octets
                :octets->int
                :octets->int64
                :octets->uint64)
  (:export :read-gtirb
           :write-gtirb
           :module
           :name
           :binary-path
           :preferred-addr
           :rebase-delta
           :isa
           :file-format
           :get-block
           :edge-type
           :edge-label
           :conditional
           :direct
           :image-byte-map
           :addr-min
           :addr-max
           :base-address
           :entry-point-address
           :regions
           :aux-data
           :aux-data-type
           :aux-data-data
           :blocks
           :section
           :sections
           :address
           :size
           :symbols
           :symbol
           :symbolic-operands
           :value
           :referent-uuid
           :storage-kind
           :proxies
           :modules
           :cfg
           :gtirb
           :update-proto))
(in-package :gtirb/gtirb)
(in-readtable :curry-compose-reader-macros)

(defun read-proto (path)
  "Read GTIRB from PATH."
  (assert (probe-file path) (path)
          "Can't read GTIRB from ~s, because the file doesn't exist."
          path)
  (let ((gtirb (make-instance 'proto-v0:ir)))
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


;;;; Classes
(defmacro define-proto-backed-class ((class proto-class) super-classes
                                     slot-specifiers proto-fields
                                     &body options)
  "Define a Common Lisp class backed by a protobuf class.
SLOT-SPECIFIERS is as in `defclass' with the addition of optional
:to-proto and :from-proto fields, which may take protobuf
serialization functions.  PROTO-FIELDS may hold a list of fields which
pass through directly to the backing protobuf class.  "
  `(progn
     (defclass ,class ,super-classes
       ;; Accessors for normal lisp classes
       ((proto :initarg :proto :accessor proto :type ,proto-class
               :initform (make-instance ',proto-class)
               :documentation "Backing protobuf object.
Should not need to be manipulated by client code.")
        ,@slot-specifiers)
       ,@options)
     ;; Pass-through accessors for protobuf fields.
     ,@(apply
        #'append
        (mapcar
         (nest
          (lambda (pair))
          (destructuring-bind
                (name &key type documentation enumeration (proto-field name)
                      &allow-other-keys) pair)
          (let ((base `(,(intern (symbol-name proto-field) 'proto-v0)
                         (proto obj)))))
          `((defmethod ,name ((obj ,class))
              ,@(when documentation (list documentation))
              ,(ecase type
                 ((unsigned-byte-64 boolean) base)
                 (enumeration `(cdr (assoc ,base ,enumeration)))
                 (uuid `(uuid-to-integer ,base))
                 (string `(pb:string-value ,base))))
            (defmethod (setf ,name) (new (obj ,class))
              ,@(when documentation (list documentation))
              ,(ecase type
                 ((unsigned-byte-64 boolean) `(setf ,base new))
                 (enumeration `(setf ,base (car (rassoc new ,enumeration))))
                 (uuid `(setf ,base (integer-to-uuid new)))
                 (string `(setf ,base (pb:string-field new)))))))
         proto-fields))))

(define-constant +module-isa-map+
    '((#.proto-v0:+isaid-isa-undefined+ . :undefined)
      (#.proto-v0:+isaid-ia32+ . :ia32)
      (#.proto-v0:+isaid-ppc32+ . :ppc32)
      (#.proto-v0:+isaid-x64+ . :x64)
      (#.proto-v0:+isaid-arm+ . :arm)
      (#.proto-v0:+isaid-valid-but-unsupported+ . :valid-but-unsupported))
  :test #'equal)

(define-constant +module-file-format-map+
    '((#.proto-v0:+file-format-coff+ . :coff)
      (#.proto-v0:+file-format-elf+ . :elf)
      (#.proto-v0:+file-format-ida-pro-db32+ . :ida-pro-db32)
      (#.proto-v0:+file-format-ida-pro-db64+ . :ida-pro-db64)
      (#.proto-v0:+file-format-macho+ . :macho)
      (#.proto-v0:+file-format-pe+ . :pe)
      (#.proto-v0:+file-format-raw+ . :raw)
      (#.proto-v0:+file-format-xcoff+ . :xcoff)
      (#.proto-v0:+file-format-format-undefined+ . :format-undefined))
  :test #'equal)

(define-proto-backed-class (module proto-v0:module) ()
    ((cfg :accessor cfg :type cfg
          :documentation "Control flow graph (CFG) keyed by UUID.")
     (blocks :accessor blocks :type hash-table
             :initform (make-hash-table)
             :documentation "Hash of code and data blocks keyed by UUID.")
     (proxies :accessor proxies :type hash-table
              :initform (make-hash-table)
              :documentation
              "Hash of proxy blocks, used to represent cross-module linkages.")
     (symbols :accessor symbols :type hash-table
              :initform (make-hash-table)
              :documentation "Hash of symbols keyed by UUID.")
     (symbolic-operands :accessor symbolic-operands :type hash-table
                        :initform (make-hash-table)
                        :documentation "Hash of symbolic-operands keyed by UUID.")
     (sections :accessor sections :type (list section)
               :documentation "Loadable sections.")
     (aux-data :accessor aux-data :type (list aux-data)
               :documentation "Auxiliary data objects.")
     (image-byte-map
      :accessor image-byte-map :type (list image-byte-map)
      :documentation
      "Collection of bytes in regions keyed by starting address."))
    ((name :type string)
     (binary-path :type string)
     (preferred-addr :type unsigned-byte-64)
     (rebase-delta :type unsigned-byte-64)
     (isa :type enumeration :enumeration +module-isa-map+ :proto-field isa-id)
     (file-format :type enumeration :enumeration +module-file-format-map+))
  (:documentation "Module of a GTIRB IR."))

(define-constant +symbol-storage-kind+
    '((#.proto-v0:+storage-kind-storage-undefined+ . :undefined)
      (#.proto-v0:+storage-kind-storage-normal+ . :normal)
      (#.proto-v0:+storage-kind-storage-static+ . :static)
      (#.proto-v0:+storage-kind-storage-extern+ . :extern)
      (#.proto-v0:+storage-kind-storage-local+ . :local))
  :test #'equal)

(define-proto-backed-class (symbol proto-v0:symbol) () ()
    ((name :type string)
     (value :type unsigned-byte-64)
     ;; TODO: Just hold the referent directly.
     (referent-uuid :type uuid)
     (storage-kind :type enumeration :enumeration +symbol-storage-kind+))
  (:documentation "Symbol."))

(defmethod print-object ((obj symbol) (stream stream))
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a ~a" (name obj) (storage-kind obj))))

(define-proto-backed-class (section proto-v0:section) () ()
    ((name :type string)
     (address :type unsigned-byte-64)
     (size :type unsigned-byte-64))
  (:documentation "Loadable section of a GTIRB IR module."))

(defmethod print-object ((obj section) (stream stream))
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a ~a ~a" (name obj) (address obj) (size obj))))

(defun aux-data-from-proto (proto)
  (let ((p-aux-data (proto-v0:aux-data (proto-v0:aux-data-container proto)))
        (aux-data '()))
    (dotimes (n (length p-aux-data))
      (push (cons (pb:string-value (proto-v0:key (aref p-aux-data n)))
                  (make-instance 'aux-data
                    :proto (proto-v0:value (aref p-aux-data n))))
            aux-data))
    aux-data))

(defun aux-data-to-proto (aux-data)
  (map 'vector (lambda (pair)
                 (destructuring-bind (name . aux-data) pair
                   (let ((entry
                          (make-instance
                              'proto-v0:aux-data-container-aux-data-entry)))
                     (setf (proto-v0:key entry) (pb:string-field name)
                           (proto-v0:value entry) (proto aux-data))
                     entry)))
       aux-data))

(defun hash-table-from-proto
    (protos &rest more-protos &aux (result (make-hash-table)))
  "Create a hash-table keyed by UUID from a array of protobuf objects."
  (let ((all (apply #'concatenate 'vector protos more-protos)))
    (dotimes (n (length all) result)
      (setf (gethash (uuid-to-integer (proto-v0:uuid (aref all n))) result)
            (aref all n)))))

(defun hash-table-to-proto (hash-table &optional filter)
  "Convert a hash-table keyed by UUID to a array for protobuf."
  (if filter
      (coerce (remove-if-not filter (hash-table-values hash-table)) 'vector)
      (coerce (hash-table-values hash-table) 'vector)))

(defmethod initialize-instance :after ((obj module) &key)
  (setf (image-byte-map obj) ; Unpack the image-byte-map.
        (make-instance 'image-byte-map
          :proto (proto-v0:image-byte-map (proto obj))))
  (setf (aux-data obj) (aux-data-from-proto (proto obj)))
  (setf (proxies obj) (hash-table-from-proto (proto-v0:proxies (proto obj))))
  ;; Package the blocks into a has keyed by UUID.
  (setf (blocks obj) (hash-table-from-proto
                      (proto-v0:blocks (proto obj)) (proto-v0:data (proto obj))))
  ;; Sections.
  (setf (sections obj)
        (map 'list {make-instance 'section :proto}
             (proto-v0:sections (proto obj))))
  ;; Symbols.
  (setf (symbols obj)
        (map 'list {make-instance 'symbol :proto}
             (proto-v0:symbols (proto obj))))
  ;; Symbolic Operands.
  (setf (symbolic-operands obj)
        (let ((p-sym-ops (proto-v0:symbolic-operands (proto obj)))
              (sym-ops-h (make-hash-table)))
          (dotimes (n (length p-sym-ops))
            (let ((p-sym-op (aref p-sym-ops n)))
              (setf (gethash (proto-v0:key p-sym-op) sym-ops-h)
                    (proto-v0:value p-sym-op))))
          sym-ops-h))
  ;; Build the CFG as a lisp graph.
  (nest
   (with-slots (cfg) obj)
   (let ((p-cfg (proto-v0:cfg (proto obj)))))
   (setf cfg)
   (populate
    (make-instance 'digraph)
    :edges-w-values
    (mapcar (lambda (edge)
              (list (list (uuid-to-integer (proto-v0:source-uuid edge))
                          (uuid-to-integer (proto-v0:target-uuid edge)))
                    (make-instance 'edge-label :proto (proto-v0:label edge))))
            (coerce (proto-v0:edges p-cfg) 'list))
    :nodes (map 'list  #'uuid-to-integer (proto-v0:vertices p-cfg)))))

(defmethod get-block ((uuid simple-array) (obj module))
  (get-block (uuid-to-integer uuid) obj))

(defmethod (setf get-block) (new (uuid simple-array) (obj module))
  (setf (get-block (uuid-to-integer uuid) obj) new))

(defmethod get-block ((uuid integer) (obj module))
  (gethash uuid (blocks obj)))

(defmethod (setf get-block) (new (uuid integer) (obj module))
  (setf (gethash uuid (blocks obj)) new))

(defun uuid-to-integer (uuid)
  (if (emptyp uuid)
      0         ; NOTE: The referent-uuid of a symbol can be #(). Bug?
      (octets->int
       (make-array 16 :element-type '(unsigned-byte 8) :initial-contents uuid)
       16)))

(defun integer-to-uuid (number)
  (int->octets number 16))

(defmethod print-object ((obj module) (stream stream))
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a ~a ~s" (file-format obj) (isa obj) (name obj))))

(define-constant +edge-label-type-map+
    '((#.proto-v0:+edge-type-type-branch+ . :branch)
      (#.proto-v0:+edge-type-type-call+ . :call)
      (#.proto-v0:+edge-type-type-fallthrough+ . :fallthrough)
      (#.proto-v0:+edge-type-type-return+ . :return)
      (#.proto-v0:+edge-type-type-syscall+ . :syscall)
      (#.proto-v0:+edge-type-type-sysret+ . :sysret))
  :test #'equal)

(define-proto-backed-class (edge-label proto-v0:edge-label) () ()
    ((conditional :type boolean)
     (direct :type boolean)
     (edge-type :type enumeration :enumeration +edge-label-type-map+
                :proto-field type))
  (:documentation "Label on a CFG edge."))

(defmethod print-object ((obj edge-label) (stream stream))
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a ~a ~a"
            (edge-type obj)
            (if (conditional obj) :conditional :unconditional)
            (if (direct obj) :direct :undirect))))

(define-proto-backed-class (image-byte-map proto-v0:image-byte-map) ()
    ((regions :initarg regions :accessor regions :initform nil
              :type (list (cons (unsigned-byte 64)
                                (simple-array (unsigned-byte 8) (*))))
              :documentation "Alist of the regions keyed by starting address."))
    ((addr-min :type unsigned-byte-64)
     (addr-max :type unsigned-byte-64)
     (base-address :type unsigned-byte-64)
     (entry-point-address :type unsigned-byte-64))
  (:documentation "Bytes of the memory image, as a list of regions."))

(defmethod initialize-instance :after ((obj image-byte-map) &key)
  (let ((p-regions (proto-v0:regions (proto-v0:byte-map (proto obj))))
        results)
    (dotimes (n (length p-regions) results)
      (let ((p-region (aref p-regions n)))
        (push (cons (proto-v0:address p-region) (proto-v0:data p-region))
              results)))
    (setf (regions obj) (nreverse results))))

(defmethod print-object ((obj image-byte-map) (stream stream))
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a to ~a in ~a regions"
            (addr-min obj) (addr-max obj) (length (regions obj)))))

(define-proto-backed-class (aux-data proto-v0:aux-data) () () ())

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
  (first (aux-data-type-read (pb:string-value (proto-v0:type-name (proto obj))))))

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

(defmethod (setf aux-data-type) (new (obj aux-data))
  (setf (proto-v0:type-name (proto obj))
        (pb:string-field (aux-data-type-print new))))

(defmethod aux-data-data ((obj aux-data))
  (aux-data-decode (aux-data-type obj) (proto-v0:data (proto obj))))

(defmethod (setf aux-data-data) (new (obj aux-data))
  (setf (proto-v0:data (proto obj))
        (let ((result (aux-data-encode (aux-data-type obj) new)))
          (make-array (length result)
                      :element-type '(unsigned-byte 8)
                      :initial-contents result))))

(defvar *decode-data* nil)
(defun advance (n) (setf *decode-data* (subseq *decode-data* n)))
(defun decode (type)
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
     (prog1 (uuid-to-integer (subseq *decode-data* 0 16)) (advance 16)))
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
     (mapcar #'decode types))))
(defun aux-data-decode (type data)
  (let ((*decode-data* data))
    (decode type)))

(defun extend (it) (push it *decode-data*))
(defun encode (type data)
  (match type
    ((or :addr :uint64-t :int64-t)
     (extend (int->octets data 8)))
    (:uuid
     (extend (integer-to-uuid data)))
    (:offset
     (progn (encode :uuid (first data))
            (encode :uint64-t (second data))))
    (:string
     (let ((string-bytes (string-to-utf-8-bytes data)))
       (encode :uint64-t (length string-bytes))
       (extend string-bytes)))
    ((list :mapping key-t value-t)
     (encode :uint64-t (hash-table-count data))
     (maphash (lambda (key value)
                (encode key-t key)
                (encode value-t value))
              data))
    ((list (or :sequence :set) type)
     (let ((size (length data)))
       (encode :uint64-t size)
       (dotimes (n size)
         (encode type (elt data n)))))
    ((list* :tuple types)
     (mapc (lambda (type datum)
             (encode type datum))
           types data))))
(defun aux-data-encode (type data)
  (let ((*decode-data* nil))
    (encode type data)
    (reduce {concatenate 'vector} (reverse *decode-data*))))

(define-proto-backed-class (gtirb proto-v0:ir) ()
    ((modules :initarg modules :accessor modules :type (list module)
              :initform nil
              :documentation "List of the modules on an IR.")
     (aux-data :accessor aux-data :type (list aux-data)
               :documentation "Auxiliary data objects on the IR.
The modules of the IR will often also hold auxiliary data objects."))
    ()
  (:documentation "Base class of an instance of GTIRB IR."))

(defmethod (setf modules) :after (new (obj gtirb))
  (setf (proto-v0:modules (proto obj))
        (coerce (mapcar #'proto (modules obj)) 'vector)))

(defmethod initialize-instance :after ((obj gtirb) &key)
  (setf (aux-data obj) (aux-data-from-proto (proto obj)))
  (with-slots (modules) obj
    (setf modules (mapcar (lambda (module-proto)
                            (make-instance 'module :proto module-proto))
                          (coerce (proto-v0:modules (proto obj)) 'list)))))

(defmethod print-object ((obj gtirb) (stream stream))
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a" (modules obj))))

(defgeneric update-proto (object)
  (:documentation
   "Dump an updated protocol buffer for OBJECT.
This will ensure that any changes made to objects outside of the
protocol buffer object, e.g. blocks on modules, are synchronized
against the protocol buffer object before it is returned.  We could
incrementally synchronize everything to the backing protocol buffer,
but that would likely get expensive.")
  (:method ((obj gtirb))
    (setf (proto-v0:modules (proto obj))   ; Modules.
          (map 'vector [#'proto #'update-proto] (modules obj))
          (proto-v0:aux-data (proto-v0:aux-data-container (proto obj))) ; Aux data.
          (aux-data-to-proto (aux-data obj))))
  (:method ((obj module))
    (setf
     ;; Repackage the ImageByteMap.
     (proto-v0:regions (proto-v0:byte-map (proto (image-byte-map obj))))
     (map 'vector (lambda (region)
                    (let ((p-region (make-instance 'proto-v0:region)))
                      (setf (proto-v0:address p-region) (car region)
                            (proto-v0:data p-region) (cdr region))
                      p-region))
          (regions (image-byte-map obj)))
     ;; Repackage the AuxData into a vector.
     (proto-v0:aux-data (proto-v0:aux-data-container (proto obj)))
     (aux-data-to-proto (aux-data obj))
     ;; Repackage the proxies into a vector.
     (proto-v0:proxies (proto obj)) (hash-table-to-proto (proxies obj))
     ;; Repackage the blocks back into a vector.
     (proto-v0:blocks (proto obj))
     (hash-table-to-proto (blocks obj) [{eql 'proto-v0:block} #'type-of])
     (proto-v0:data (proto obj))
     (hash-table-to-proto (blocks obj) [{eql 'proto-v0:data-object} #'type-of])
     ;; Repackage the sections back into a vector.
     (proto-v0:sections (proto obj))
     (map 'vector #'proto (sections obj))
     ;; Repackage the symbols back into a vector.
     (proto-v0:symbols (proto obj))
     (map 'vector #'proto (symbols obj))
     ;; Repackage the symbolic operands.
     (proto-v0:symbolic-operands (proto obj))
     (map 'vector
          (lambda (pair)
            (destructuring-bind (key . value) pair
              (let ((sym-op-entry
                     (make-instance 'proto-v0::module-symbolic-operands-entry)))
                (setf (proto-v0:key sym-op-entry) key
                      (proto-v0:value sym-op-entry) value)
                sym-op-entry)))
          (hash-table-alist (symbolic-operands obj)))
     ;; Unpack the graph back into the proto structure.
     (proto-v0:cfg (proto obj))
     (let ((p-cfg (make-instance 'proto-v0:cfg)))
       (setf (proto-v0:vertices p-cfg)
             (map 'vector #'integer-to-uuid (nodes (cfg obj)))
             (proto-v0:edges p-cfg)
             (map 'vector
                  (lambda (edge)
                    (destructuring-bind ((source target) label) edge
                      (let ((p-edge (make-instance 'proto-v0:edge)))
                        (setf
                         (proto-v0:source-uuid p-edge) (integer-to-uuid source)
                         (proto-v0:target-uuid p-edge) (integer-to-uuid target)
                         (proto-v0:label p-edge) (proto label))
                        p-edge)))
                  (edges-w-values (cfg obj))))
       p-cfg))
    obj))

(defun read-gtirb (path)
  (make-instance 'gtirb :proto (read-proto path)))

(defun write-gtirb (gtirb path)
  (update-proto gtirb)
  (write-proto (proto gtirb) path))

#+(or )
(progn
  (defclass byte-interval ()
    ((bytes
      :accessor bytes
      :initform (make-array 0 :element-type '(unsigned-byte 8))
      :type (simple-array (unsigned-byte 8) (*)))
     (uuid
      :accessor uuid
      ;; In real life this would be random.
      :initform (make-array 0 :element-type '(unsigned-byte 8))
      :type (simple-array (unsigned-byte 8) (*)))))

  (defclass block ()
    ((uuid
      :accessor uuid
      ;; In real life this would be random.
      :initform (make-array 0 :element-type '(unsigned-byte 8))
      :type (simple-array (unsigned-byte 8) (*)))))

  (defclass cfg-node (block) ())

  (defclass byte-block (block)
    ((byte-interval-uuid
      :accessor byte-interval-uuid
      :initform (uuid (make-instance 'byte-interval))
      :type (simple-array (unsigned-byte 8) (*)))
     (byte-offset
      :accessor byte-offset
      :initform 0
      :type fixnum)))

  (defclass data-block (byte-block) ())
  (defclass code-block (cfg-node byte-block) ())

  ;; GTIRB> (describe (make-instance 'data-block))
  ;; #<DATA-BLOCK {10056207F3}>
  ;;   [standard-object]
  ;;
  ;; Slots with :INSTANCE allocation:
  ;;   UUID                           = #()
  ;;   BYTE-INTERVAL-UUID             = #()
  ;;   BYTE-OFFSET                    = 0
  ;; ; No value
  ;; GTIRB> (describe (make-instance 'code-block))
  ;; #<CODE-BLOCK {1005624343}>
  ;;   [standard-object]
  ;;
  ;; Slots with :INSTANCE allocation:
  ;;   UUID                           = #()
  ;;   BYTE-INTERVAL-UUID             = #()
  ;;   BYTE-OFFSET                    = 0
  ;; ; No value
  ;; GTIRB> (subclassp (find-class 'code-block) (find-class 'cfg-node))
  ;; T
  ;; GTIRB> (subclassp (find-class 'data-block) (find-class 'cfg-node))
  ;; NIL
  ;; GTIRB>
  )
