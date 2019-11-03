(defpackage :gtirb/gtirb
  (:nicknames :gtirb)
  (:use :common-lisp :alexandria :graph :trivia
        :trivial-utf-8
        :named-readtables :curry-compose-reader-macros)
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
  (let ((gtirb (make-instance 'proto:ir)))
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
(defgeneric is-equal-p (left right)
  (:documentation "Return t if LEFT and RIGHT are equal."))

;;; TODO: Add type setters and getter through enumerations, then
;;;       complete the rest of the defclass conversions and test.
;;;       Then complete the is-equal-p pieces.
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
        ,@(mapcar «cons #'car [{remove-from-plist _ :is-equal-p} #'cdr]»
                  slot-specifiers))
       ,@options)
     ;; Equality.
     ,@(apply #'append
              (mapcar
               (lambda (specification)
                 (destructuring-bind (field &key is-equal-p &allow-other-keys)
                     specification
                   (when is-equal-p
                     (let ((type (getf specification :type)))
                       (assert type (specification)
                               "Defining `is-equal-p' for ~a requires a type."
                               field)
                       `((defmethod is-equal-p ((left ,class) (right ,class))
                           (apply ,is-equal-p left right)))))))
               (append slot-specifiers proto-fields)))
     ;; Pass-through accessors for protobuf fields.
     ,@(apply #'append
              (mapcar
               (lambda (pair)
                 (destructuring-bind
                       (name &key type documentation &allow-other-keys) pair
                   (let ((base
                          `(,(intern (symbol-name name) 'proto) (proto obj))))
                     `((defmethod ,name ((obj ,proto-class))
                         ,@(when documentation (list documentation))
                         ,(ecase type
                            ((unsigned-byte-64 boolean) base)
                            (string `(pb:string-value ,base))))
                       (defmethod (setf ,name) (new (obj ,proto-class))
                         ,@(when documentation (list documentation))
                         ,(ecase type
                            ((unsigned-byte-64 boolean) `(setf ,base new))
                            (string `(setf ,base (pb:string-field new)))))))))
               proto-fields))))

(defclass module ()
  ;; TODO: Remaining fields:
  ;; - symbols
  ;; - data
  ;; - proxies
  ;; - sections
  ;; - symbolic-operands
  ((proto :initarg :proto :accessor proto :type proto:module
          :documentation "Backing protobuf object.
Should not need to be manipulated by client code.")
   (cfg :accessor cfg :type cfg
        :documentation "Control flow graph (CFG) keyed by UUID.")
   (blocks :accessor blocks :type hash-table
           :documentation "Hash of code blocks keyed by UUID.")
   (aux-data :accessor aux-data :type (list aux-data)
             :documentation "Auxiliary data objects.")
   (image-byte-map
    :accessor image-byte-map :type (list image-byte-map)
    :documentation
    "Collection of bytes in regions keyed by starting address.")))

(define-proto-accessors module
    ((name :string)
     (binary-path :string)
     (preferred-addr :unsigned-byte-64)
     (rebase-delta :unsigned-byte-64)))

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

(defun aux-data-from-proto (proto)
  (let ((p-aux-data (proto:aux-data (proto:aux-data-container proto)))
        (aux-data '()))
    (dotimes (n (length p-aux-data))
      (push (cons (pb:string-value (proto:key (aref p-aux-data n)))
                  (make-instance 'aux-data
                    :proto (proto:value (aref p-aux-data n))))
            aux-data))
    aux-data))

(defmethod aux-data-to-proto (aux-data)
  (map 'vector (lambda (pair)
                 (destructuring-bind (name . aux-data) pair
                   (let ((entry
                          (make-instance
                              'proto:aux-data-container-aux-data-entry)))
                     (setf (proto:key entry) (pb:string-field name)
                           (proto:value entry) (proto aux-data))
                     entry)))
       aux-data))

(defmethod initialize-instance :after ((obj module) &key)
  ;; Unpack the image-byte-map.
  (setf (image-byte-map obj)
        (make-instance 'image-byte-map
          :proto (proto:image-byte-map (proto obj))))
  ;; Repackage the AuxData into an alist keyed by name.
  (setf (aux-data obj) (aux-data-from-proto (proto obj)))
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
  (octets->int
   (make-array 16 :element-type '(unsigned-byte 8) :initial-contents uuid)
   16))

(defun integer-to-uuid (number)
  (int->octets number 16))

(defmethod print-object ((obj module) (stream stream))
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a ~a ~s" (file-format obj) (isa obj) (name obj))))

(define-proto-backed-class (edge-label edge-label) () ()
    ((conditional :type boolean)
     (direct :type boolean))
  (:documentation "Label on a CFG edge."))

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

(defmethod print-object ((obj edge-label) (stream stream))
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a ~a ~a"
            (edge-type obj)
            (if (conditional obj) :conditional :unconditional)
            (if (direct obj) :direct :undirect))))

(define-proto-backed-class (image-byte-map image-byte-map) ()
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
  (let ((p-regions (proto:regions (proto:byte-map (proto obj))))
        results)
    (dotimes (n (length p-regions) results)
      (let ((p-region (aref p-regions n)))
        (push (cons (proto:address p-region) (proto:data p-region))
              results)))
    (setf (regions obj) (nreverse results))))

(defmethod print-object ((obj image-byte-map) (stream stream))
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a to ~a in ~a regions"
            (addr-min obj) (addr-max obj) (length (regions obj)))))

(define-proto-backed-class (aux-data aux-data) () () ())

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

(defmethod (setf aux-data-type) (new (obj aux-data))
  (setf (proto:type-name (proto obj))
        (pb:string-field (aux-data-type-print new))))

(defmethod aux-data-data ((obj aux-data))
  (aux-data-decode (aux-data-type obj) (proto:data (proto obj))))

(defmethod (setf aux-data-data) (new (obj aux-data))
  (setf (proto:data (proto obj))
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

(define-proto-backed-class (gtirb ir) ()
    ((modules :initarg modules :accessor modules :type (list module)
              :initform nil
              :documentation "List of the modules on an IR.")
     (aux-data :accessor aux-data :type (list aux-data)
               :documentation "Auxiliary data objects on the IR.
The modules of the IR will often also hold auxiliary data objects."))
    ()
  (:documentation "Base class of an instance of GTIRB IR."))

(defmethod (setf modules) :after (new (obj gtirb))
  (setf (proto:modules (proto obj))
        (coerce (mapcar #'proto (modules obj)) 'vector)))

(defmethod initialize-instance :after ((obj gtirb) &key)
  (setf (aux-data obj) (aux-data-from-proto (proto obj)))
  (with-slots (modules) obj
    (setf modules (mapcar (lambda (module-proto)
                            (make-instance 'module :proto module-proto))
                          (coerce (proto:modules (proto obj)) 'list)))))

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
    (setf (proto:modules (proto obj))   ; Modules.
          (map 'vector [#'proto #'update-proto] (modules obj))
          (proto:aux-data (proto:aux-data-container (proto obj))) ; Aux data.
          (aux-data-to-proto (aux-data obj))))
  (:method ((obj module))
    (setf
     ;; Repackage the ImageByteMap.
     (proto:regions (proto:byte-map (proto (image-byte-map obj))))
     (map 'vector (lambda (region)
                    (let ((p-region (make-instance 'proto:region)))
                      (setf (proto:address p-region) (car region)
                            (proto:data p-region) (cdr region))
                      p-region))
          (regions (image-byte-map obj)))
     ;; Repackage the AuxData into a vector.
     (proto:aux-data (proto:aux-data-container (proto obj)))
     (aux-data-to-proto (aux-data obj))
     ;; Repackage the blocks back into a vector.
     (proto:blocks (proto obj))
     (coerce (hash-table-values (blocks obj)) 'vector)
     ;; Unpack the graph back into the proto structure.
     (proto:cfg (proto obj))
     (let ((p-cfg (make-instance 'proto:cfg)))
       (setf (proto:vertices p-cfg)
             (map 'vector #'integer-to-uuid (nodes (cfg obj)))
             (proto:edges p-cfg)
             (map 'vector
                  (lambda (edge)
                    (destructuring-bind ((source target) label) edge
                      (let ((p-edge (make-instance 'proto:edge)))
                        (setf
                         (proto:source-uuid p-edge) (integer-to-uuid source)
                         (proto:target-uuid p-edge) (integer-to-uuid target)
                         (proto:label p-edge) (proto label))
                        p-edge)))
                  (edges-w-values (cfg obj))))
       p-cfg))
    obj))

(defun read-gtirb (path)
  (make-instance 'gtirb :proto (read-proto path)))

(defun write-gtirb (gtirb path)
  (update-proto gtirb)
  (write-proto (proto gtirb) path))
