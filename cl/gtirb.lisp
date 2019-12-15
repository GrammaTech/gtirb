(defpackage :gtirb/gtirb
  (:nicknames :gtirb)
  (:use :common-lisp :alexandria :graph :trivia
        :trivial-utf-8
        :named-readtables :curry-compose-reader-macros)
  (:shadow :symbol)
  (:import-from :proto)
  (:import-from :uiop :nest)
  (:import-from :cl-intbytes
                :int->octets
                :octets->int
                :octets->int64
                :octets->uint64)
  (:export :read-gtirb
           :write-gtirb
           :is-equal-p
           :proto-backed
           :update-proto
           :*is-equal-p-verbose-p*
;;; Classes and fields.
           :gtirb
           ;; Module
           :module
           :name
           :binary-path
           :isa
           :file-format
           :preferred-addr
           :rebase-delta
           :symbols
           :cfg
           :proxies
           :sections
           :aux-data
           ;; Symbol
           :symbol
           :value
           :referent-uuid
           :storage-kind
           ;; Section
           :section
           :byte-intervals
           ;; Byte-Interval
           :byte-interval
           :blocks
           :symbolic-expressions
           :addressp
           :address
           :contents
           :size
           ;; Block
           :gtirb-block
           :code-block
           :data-block
           :decode-mode
           :bytes
           ;; Edge-Label
           :edge-label
           :conditional
           :direct
           :edge-type
           ;; Aux-Data
           :aux-data-type
           :aux-data-data
           ;; gtirb
           :modules
;;; Additional methods.
           :get-block))
(in-package :gtirb/gtirb)
(in-readtable :curry-compose-reader-macros)

(defun read-proto (path)
  "Read raw GTIRB IR PROTOBUF from PATH."
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
  "Write RAW GTIRB IR PROTOBUF to PATH."
  (let* ((size (pb:octet-size gtirb))
         (buffer (make-array size :element-type '(unsigned-byte 8))))
    (pb:serialize gtirb buffer 0 size)
    (with-open-file (output path
                            :direction :output :if-exists :supersede
                            :element-type 'unsigned-byte)
      (write-sequence buffer output)))
  (values))

(defun read-gtirb (path)
  "Read a GTIRB IR object from PATH."
  (make-instance 'gtirb :proto (read-proto path)))

(defun write-gtirb (gtirb path)
  "Write a GTIRB IR object to PATH."
  (update-proto gtirb)
  (write-proto (proto gtirb) path))


;;;; Class utilities.
(defun uuid-to-integer (uuid)
  (if (emptyp uuid)
      ;;TODO: Needed when referent-uuid of a symbol can be #().
      ;;      Remove this case and instead stop processing missing
      ;;      referent-uuids.
      0
      (octets->int
       (make-array 16 :element-type '(unsigned-byte 8) :initial-contents uuid)
       16)))

(defun integer-to-uuid (number)
  (int->octets number 16))

(defvar *is-equal-p-verbose-p* nil
  "Compare equality verbosely.")

(defvar *is-equal-p-verbose-output-buffer* nil
  "Buffer to hold output of is-equal-p verbose failure messages.")

(defvar *is-equal-p-verbose-output-length* 10
  "Maximum length of output to show of `*is-equal-p-verbose-output-buffer*'.")

(defmacro compare-or-verbose (comparison left right &rest flags)
  `(or (,comparison ,left ,right ,@flags)
       (prog1 nil
         (when *is-equal-p-verbose-p*
           (push (format nil "NOT ~S" (list ',comparison ,left ,right))
                 *is-equal-p-verbose-output-buffer*)))))

(defun is-equal-p (left right)
  "Return t if LEFT and RIGHT are equal."
  (let ((*is-equal-p-verbose-output-buffer* nil))
    (let ((equalp (is-equal-p-internal left right)))
      (prog1 equalp
        (when (and (not equalp) *is-equal-p-verbose-p*)
          (format t "~{~S~%~}"
                  (subseq *is-equal-p-verbose-output-buffer*
                          0 *is-equal-p-verbose-output-length*)))))))

(defmethod :around is-equal-p-internal ((left t) (right t))
  (let ((equalp (call-next-method)))
    (when equalp (setf *is-equal-p-verbose-output-buffer* nil))
    equalp))

(defgeneric is-equal-p-internal (left right)
  (:documentation "Internal function called by `is-equal-p'.")
  (:method ((left t) (right t))
    (compare-or-verbose equalp left right))
  (:method ((left number) (right number))
    (compare-or-verbose = left right))
  (:method ((left cl:symbol) (right cl:symbol))
    (compare-or-verbose eql left right))
  (:method ((left string) (right string))
    (compare-or-verbose string= left right))
  (:method ((left cons) (right cons))
    (if (and (proper-list-p left) (proper-list-p right))
        (compare-or-verbose set-equal left right :test #'is-equal-p-internal)
        (and (compare-or-verbose is-equal-p-internal (car left) (car right))
             (compare-or-verbose is-equal-p-internal (cdr left) (cdr right)))))
  (:method ((left hash-table) (right hash-table))
    (compare-or-verbose set-equal
                        (hash-table-alist left) (hash-table-alist right)
                        :test #'is-equal-p-internal))
  (:method ((left graph:digraph) (right graph:digraph))
    (and (compare-or-verbose set-equal (graph:nodes left) (graph:nodes right)
                             :test #'is-equal-p-internal)
         (compare-or-verbose set-equal (graph:edges-w-values left)
                             (graph:edges-w-values right)
                             :test #'is-equal-p-internal))))

(defclass proto-backed () ()
  (:documentation "Objects which may be serialized to/from protobuf."))

(defgeneric get-uuid (uuid object)
  (:documentation "Get the referent of UUID in OBJECT."))

(defgeneric (setf get-uuid) (uuid object new)
  (:documentation "Register REFERENT behind UUID in OBJECT."))

(defgeneric update-proto (proto-backed-object)
  (:documentation
   "Update the `proto' field of OBJECT and return the updated value.
This will ensure that any changes made to OBJECT outside of its
protocol buffer, e.g. any slots initialized using the :from-proto
option to `define-proto-backed-class', are synchronized against the
object's protocol buffer.")
  (:method ((proto-backed-object proto-backed))
    (proto proto-backed-object)))

(defmacro define-proto-backed-class ((class proto-class) super-classes
                                     slot-specifiers proto-fields
                                     &rest options)
  "Define a Common Lisp class backed by a protobuf class.
SLOT-SPECIFIERS is as in `defclass' with the addition of optional
:to-proto and :from-proto fields, which may take protobuf
serialization functions, and :skip-equal-p field which causes
`is-equal-p' to skip that field.  PROTO-FIELDS may hold a list of
fields which pass through directly to the backing protobuf class.

New :parent option names the field holding the containing protobuf
element."
  (nest
   (flet ((plist-get (item list)
            (second (member item list)))
          (plist-drop (item list)
            (if-let ((location (position item list)))
              (append (subseq list 0 location)
                      (subseq list (+ 2 location)))
              list))))
   (let ((from-proto-slots (remove-if-not {find :from-proto} slot-specifiers))
         (to-proto-slots (remove-if-not {find :to-proto} slot-specifiers))
         (parent (second (assoc :parent options)))))
   `(progn
      (defclass ,class (proto-backed ,@super-classes)
        ;; Accessors for normal lisp classes
        ((proto :initarg :proto :accessor proto :type ,proto-class
                :initform (make-instance ',proto-class)
                :documentation "Backing protobuf object.
Should not need to be manipulated by client code.")
         ,@(when parent
             `((,parent :accessor ,parent :type ,parent
                        :initarg ,(make-keyword parent)
                        :initform (error ,(format nil "~a created without a ~
                                                      pointer to enclosing ~a."
                                                  class parent))
                        :documentation ,(format nil "Access the ~a of this ~a."
                                                parent class))))
         ,@(mapcar [{plist-drop :to-proto} {plist-drop :from-proto}
                    {plist-drop :proto-field} {plist-drop :skip-equal-p}]
                   slot-specifiers))
        ,@(remove-if [{eql :parent} #'car] options))
      ,@(when parent
          `((defmethod get-uuid (uuid (object ,class))
              (get-uuid (,parent object) uuid))
            (defmethod (setf get-uuid) (uuid (object ,class) new)
              (setf (get-uuid (,parent object) uuid) new))))
      (defmethod initialize-instance :after ((self ,class) &key)
                 ,@(when parent
                     `((setf (get-uuid self (proto:uuid (proto self))) self)))
                 (with-slots (proto ,@(mapcar #'car from-proto-slots)) self
                   ,@(mapcar
                      (lambda (spec)
                        (destructuring-bind (slot &key from-proto &allow-other-keys) spec
                          `(setf ,slot (funcall ,from-proto proto))))
                      from-proto-slots)))
      (defmethod update-proto ((self ,class))
        ,@(mapcar
           (lambda (spec)
             (destructuring-bind
                   (slot &key to-proto (proto-field slot) &allow-other-keys)
                 spec
               `(setf (,(intern (symbol-name proto-field) 'proto) (proto self))
                      (funcall ,to-proto (,slot self)))))
           to-proto-slots)
        (proto self))
      ;; Equality check on class.
      ;;
      ;; NOTE: For this to work we might need to add an optional :only
      ;;       field to both slot-specifiers  and proto-fields.  This
      ;;       would mean that the equality of this field is only
      ;;       checked with this form returns true.  E.g., on
      ;;       byte-intervals we could say:
      ;;       (address :type unsigned-byte-64 :only #'addressp)
      (defmethod is-equal-p-internal ((left ,class) (right ,class))
        (and ,@(mapcar
                (lambda (accessor)
                  `(compare-or-verbose is-equal-p-internal
                                       (,accessor left) (,accessor right)))
                (append
                 (mapcar {plist-get :accessor}
                         (remove-if {plist-get :skip-equal-p}
                                    slot-specifiers))
                 (mapcar #'car proto-fields)))))
      ;; Pass-through accessors for protobuf fields so they operate
      ;; directly on the backing protobuf object.
      ,@(apply
         #'append
         (mapcar
          (nest
           (lambda (pair))
           (destructuring-bind
                 (name &key type documentation enumeration (proto-field name)
                       &allow-other-keys) pair)
           (let ((base `(,(intern (symbol-name proto-field) 'proto)
                          (proto obj)))))
           `((defmethod ,name ((obj ,class))
               ,@(when documentation (list documentation))
               ,(ecase type
                  ((unsigned-byte-64 boolean bytes) base)
                  (enumeration `(cdr (assoc ,base ,enumeration)))
                  (uuid `(uuid-to-integer ,base))
                  (string `(pb:string-value ,base))))
             (defmethod (setf ,name) (new (obj ,class))
               ,@(when documentation (list documentation))
               ,(ecase type
                  ((unsigned-byte-64 boolean) `(setf ,base new))
                  (bytes `(setf ,base
                                (make-array (length new)
                                            :element-type '(unsigned-byte 8)
                                            :initial-contents new)))
                  (enumeration `(setf ,base (car (rassoc new ,enumeration))))
                  (uuid `(setf ,base (integer-to-uuid new)))
                  (string `(setf ,base (pb:string-field new)))))))
          proto-fields)))))


;;;; Classes.
(define-proto-backed-class (gtirb proto:ir) ()
    ((modules :initarg modules :accessor modules :type (list module)
              :initform nil
              :from-proto
              (lambda (proto)
                (mapcar
                 (lambda (module-proto)
                   (make-instance 'module :gtirb self :proto module-proto))
                 (coerce (proto:modules proto) 'list)))
              :to-proto
              (lambda (modules) (map 'vector #'update-proto modules))
              :documentation "List of the modules on an IR.")
     (aux-data :accessor aux-data :type (list aux-data)
               :from-proto #'aux-data-from-proto
               :to-proto #'aux-data-to-proto
               :documentation "Auxiliary data objects on the IR.
The modules of the IR will often also hold auxiliary data objects.")
     (by-uuid :accessor by-uuid :initform (make-hash-table) :type hash-table
              :skip-equal-p t
              :documentation "Internal cache for UUID-based lookup.")
     #| TODO: Cache for quick lookup for blocks by ADDRESS? |#)
    ()
  (:documentation "Base class of an instance of GTIRB IR."))

(defmethod print-object ((obj gtirb) (stream stream))
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a" (modules obj))))

(defmethod get-uuid (uuid (obj gtirb))
  (gethash uuid (by-uuid obj)))

(defmethod (setf get-uuid) (uuid (obj gtirb) new)
  (setf (gethash uuid (by-uuid obj)) new))

(define-constant +module-isa-map+
    '((#.proto:+isa-isa-undefined+ . :undefined)
      (#.proto:+isa-ia32+ . :ia32)
      (#.proto:+isa-ppc32+ . :ppc32)
      (#.proto:+isa-x64+ . :x64)
      (#.proto:+isa-arm+ . :arm)
      (#.proto:+isa-valid-but-unsupported+ . :valid-but-unsupported))
  :test #'equal)

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

(define-proto-backed-class (module proto:module) ()
    ((cfg :accessor cfg :type digraph
          :from-proto
          (lambda (proto)
            (let ((p-cfg (proto:cfg proto)))
              (populate
               (make-instance 'digraph)
               :edges-w-values
               (mapcar
                (lambda (edge)
                  (list (list (uuid-to-integer (proto:source-uuid edge))
                              (uuid-to-integer (proto:target-uuid edge)))
                        (make-instance 'edge-label :proto (proto:label edge))))
                (coerce (proto:edges p-cfg) 'list))
               :nodes (map 'list  #'uuid-to-integer (proto:vertices p-cfg)))))
          :to-proto
          (lambda (cfg &aux (p-cfg (make-instance 'proto:cfg)))
            (setf
             (proto:vertices p-cfg)
             (map 'vector #'integer-to-uuid (nodes cfg))
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
                  (edges-w-values cfg)))
            p-cfg)
          :documentation "Control flow graph (CFG) keyed by UUID.")
     (proxies :accessor proxies :type hash-table
              :initform (make-hash-table)
              :from-proto
              (lambda (proto &aux (table (make-hash-table)))
                (let ((proto-proxies (proto:proxies proto)))
                  (dotimes (n (length proto-proxies) table)
                    (let ((it (aref proto-proxies n)))
                      (setf (gethash (uuid-to-integer (proto:uuid it)) table)
                            (make-instance 'proxy-block
                              :module self :proto it))))))
              :to-proto
              (lambda (proxies)
                (map 'vector (lambda (uuid)
                               (let ((it (make-instance 'proto:proxy-block)))
                                 (setf (proto:uuid it) (integer-to-uuid uuid))
                                 it))
                     (mapcar #'car (hash-table-alist proxies))))
              :documentation
              "Hash of proxy blocks, used to represent cross-module linkages.")
     (symbols :accessor symbols :type hash-table
              :initform (make-hash-table)
              :from-proto (lambda (proto) (map 'list {make-instance 'symbol :proto}
                                               (proto:symbols proto)))
              :to-proto (lambda (symbols) (map 'vector #'update-proto symbols))
              :documentation "Hash of symbols keyed by UUID.")
     (sections :accessor sections :type (list section)
               :from-proto
               (lambda (proto)
                 (map 'list {make-instance 'section :module self :proto}
                      (proto:sections proto)))
               :to-proto (lambda (sections) (map 'vector #'update-proto sections))
               :documentation "GTIRB sections.")
     (aux-data :accessor aux-data :type (list aux-data)
               :from-proto #'aux-data-from-proto
               :to-proto #'aux-data-to-proto
               :documentation "Auxiliary data objects."))
    ((name :type string)
     (binary-path :type string)
     (preferred-addr :type unsigned-byte-64)
     (rebase-delta :type unsigned-byte-64)
     (isa :type enumeration :enumeration +module-isa-map+)
     (file-format :type enumeration :enumeration +module-file-format-map+))
  (:documentation "Module of a GTIRB IR instance.") (:parent gtirb))

(defmethod print-object ((obj module) (stream stream))
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a ~a ~s" (file-format obj) (isa obj) (name obj))))

(define-constant +edge-label-type-map+
    '((#.proto:+edge-type-type-branch+ . :branch)
      (#.proto:+edge-type-type-call+ . :call)
      (#.proto:+edge-type-type-fallthrough+ . :fallthrough)
      (#.proto:+edge-type-type-return+ . :return)
      (#.proto:+edge-type-type-syscall+ . :syscall)
      (#.proto:+edge-type-type-sysret+ . :sysret))
  :test #'equal)

;;; TODO: Everything needs to be able to walk up to the TOP-level IR.
;;;       In particular some things are currently homeless.  Like
;;;       edge-labels.  Probably it makes sense to add a PARENT
;;;       parameter to the `define-proto-backed-class' macro.  This
;;;       will allow that macro to populate some of the boilerplate,
;;;       like the `get-uuid' defmethods.

(define-proto-backed-class (edge-label proto:edge-label) () ()
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

(define-constant +symbol-storage-kind+
    '((#.proto:+storage-kind-storage-undefined+ . :undefined)
      (#.proto:+storage-kind-storage-normal+ . :normal)
      (#.proto:+storage-kind-storage-static+ . :static)
      (#.proto:+storage-kind-storage-extern+ . :extern)
      (#.proto:+storage-kind-storage-local+ . :local))
  :test #'equal)

(define-proto-backed-class (symbol proto:symbol) () ()
    ((name :type string)
     (value :type unsigned-byte-64)
     (referent-uuid :type uuid) ; TODO: Just hold the referent directly.
     (storage-kind :type enumeration :enumeration +symbol-storage-kind+))
  (:documentation
   "Symbol with it's NAME, STORAGE-KIND, and either VALUE or REFERENT."))

(defmethod print-object ((obj symbol) (stream stream))
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a ~a" (name obj) (storage-kind obj))))

(define-proto-backed-class (section proto:section) ()
    ((byte-intervals
      :accessor byte-intervals :type (list byte-interval)
      :from-proto
      (lambda (proto)
        (map 'list {make-instance 'byte-interval :section self :proto}
             (proto:byte-intervals proto)))
      :to-proto (lambda (byte-intervals) (map 'vector #'update-proto byte-intervals))
      :documentation "Byte-intervals."))
    ((name :type string))
  (:documentation "Section in a GTIRB IR instance.") (:parent module))

(defmethod print-object ((obj section) (stream stream))
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a ~a ~a" (name obj) (address obj) (size obj))))

(define-proto-backed-class (byte-interval proto:byte-interval) ()
    ;; TODO: What's a better data structure to use to store a sorted
    ;;       collection of pairs which permits duplicates.  Maybe a
    ;;       balanced tree.
    ((blocks :accessor blocks :type (list gtirb-block)
             :from-proto
             (lambda (proto)
               (map 'list
                    (lambda (proto-block)
                      (let* ((data (proto:data proto-block))
                             (it (etypecase data
                                   (proto:code-block
                                    (make-instance 'code-block
                                      :byte-interval self :proto data))
                                   (proto:data-block
                                    (make-instance 'data-block
                                      :byte-interval self :proto data)))))
                        (setf (offset it) (proto:offset proto-block))
                        it))
                    (proto:blocks proto)))
             :to-proto
             (lambda (blocks)
               (map 'vector (lambda (gtirb-block)
                              (let ((it (make-instance 'proto:block)))
                                (setf (proto:offset it) (offset gtirb-block))
                                (etypecase gtirb-block
                                  (code-block
                                   (setf (proto:code it)
                                         (update-proto gtirb-block)))
                                  (data-block
                                   (setf (proto:data it)
                                         (update-proto gtirb-block))))
                                it))
                    blocks))
             :documentation "Blocks in this byte-interval.")
     (symbolic-expressions
      :accessor symbolic-expressions :type hash-table
      :from-proto
      (lambda (proto &aux (table (make-hash-table)))
        (dotimes (n (length (proto:symbolic-expressions proto)) table)
          (let* ((proto (aref (proto:symbolic-expressions proto) n))
                 (address (proto:key proto))
                 (symbolic-expression (proto:value proto)))
            (setf (gethash address table)
                  (cond
                    ((proto:stack-const symbolic-expression)
                     (make-instance 'sym-stack-const
                       :proto (proto:stack-const symbolic-expression)))
                    ((proto:addr-const symbolic-expression)
                     (make-instance 'sym-addr-const
                       :proto (proto:addr-const symbolic-expression)))
                    ((proto:addr-addr symbolic-expression)
                     (make-instance 'sym-addr-addr
                       :proto (proto:addr-addr symbolic-expression))))))))
      :to-proto
      (lambda (symbolic-expression)
        (map 'vector
             (lambda (pair)
               (destructuring-bind (address . symbolic-expression) pair
                 (let ((it (make-instance
                               'proto:byte-interval-symbolic-expressions-entry)))
                   (setf (proto:key it) address
                         (proto:value it)
                         (let ((it (make-instance 'proto:symbolic-expression)))
                           (etypecase symbolic-expression
                             (sym-stack-const
                              (setf (proto:stack-const it)
                                    (proto symbolic-expression)))
                             (sym-addr-const
                              (setf (proto:addr-const it)
                                    (proto symbolic-expression)))
                             (sym-addr-addr
                              (setf (proto:addr-addr it)
                                    (proto symbolic-expression))))
                           it))
                   it)))
             (hash-table-alist symbolic-expression)))
      :documentation "Hash of symbolic-expressions keyed by address."))
    ((addressp :type boolean :proto-field has-address)
     (address :type unsigned-byte-64)
     (size :type unsigned-byte-64)
     (contents :type bytes))
  (:documentation "Byte-interval in a GTIRB instance.") (:parent section))

(defmethod print-object ((obj byte-interval) (stream stream))
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a ~a"
            (if (addressp obj) (address obj) "?")
            (size obj))))

(defclass symbolic-expression ()
  ((symbols :accessor symbols :type (list symbol)
            :documentation "Symbol(s) appearing in this symbolic expression.")))

(defmethod print-object ((obj symbolic-expression) (stream stream))
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a ~{~a~^, ~}" (offset obj) (symbols obj))))

(define-proto-backed-class
    (sym-stack-const proto:sym-stack-const) (symbolic-expression) ()
    ((offset :type unsigned-byte-64)))

(define-proto-backed-class
    (sym-addr-const proto:sym-addr-const) (symbolic-expression) ()
    ((offset :type unsigned-byte-64)))

(define-proto-backed-class
    (sym-addr-addr proto:sym-addr-addr) (symbolic-expression) ()
    ((offset :type unsigned-byte-64)
     (scale :type unsigned-byte-64)))

(defclass gtirb-block () ())

(defclass gtirb-byte-block (gtirb-block) ())

(defgeneric bytes (object)
  (:documentation "Return the bytes for OBJECT.")
  (:method ((obj gtirb-byte-block))
    #+debug (format t "[~S] ~S:[~S:~S]<-[~S:~S]~%"
                    (proto:uuid (proto obj))
                    (name (section (byte-interval obj)))
                    (or (and (addressp (byte-interval obj))
                             (address (byte-interval obj)))
                        "?")
                    (size (byte-interval obj))
                    (offset obj) (size obj))
    (let ((start (offset obj))
          (end (+ (offset obj) (size obj))))
      (assert (<= end (size (byte-interval obj))) (obj)
              "Block's end ~d exceeds size of containing byte-interval ~d."
              end (size (byte-interval obj)))
      (let ((real-end (length (contents (byte-interval obj)))))
        (cond
          ((<= end real-end)            ; Allocated bytes.
           (subseq (contents (byte-interval obj)) start end))
          ((<= start real-end) ; Both allocated and un-allocated bytes.
           (concatenate 'vector (subseq (contents (byte-interval obj)) start)
                        (make-array (- end real-end) :initial-element 0)))
          (t                          ; Un-allocated bytes, zero-fill.
           (make-array (size obj) :initial-element 0)))))))

(define-proto-backed-class (code-block proto:code-block) (gtirb-byte-block)
    ((offset :initarg :offset :accessor offset :type number
             :initform 0
             :documentation
             "Offset into this block's bytes in the block's byte-interval."))
    ((size :type unsigned-byte-64)
     (decode-mode :type unsigned-byte-64))
  (:documentation "Code-block in a GTIRB IR instance.")
  (:parent byte-interval))

(defmethod print-object ((obj code-block) (stream stream))
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a ~a" (size obj) (decode-mode obj))))

(define-proto-backed-class (data-block proto:data-block) (gtirb-byte-block)
    ((offset :initarg :offset :accessor offset :type number
             :initform 0
             :documentation
             "Offset into this block's bytes in the block's byte-interval."))
    ((size :type unsigned-byte-64))
  (:documentation "Data-block in a GTIRB IR instance.")
  (:parent byte-interval))

(defmethod print-object ((obj data-block) (stream stream))
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a" (size obj))))

(define-proto-backed-class (proxy-block proto:proxy-block) (gtirb-block) () ()
  (:documentation "Proxy-block in a GTIRB IR instance.")
  (:parent module))

(defmethod print-object ((obj proxy-block) (stream stream))
  (print-unreadable-object (obj stream :type t :identity t)))


;;;; AuxData type and data handling.
(define-proto-backed-class (aux-data proto:aux-data) () () ())

(defmethod print-object ((obj aux-data) (stream stream))
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a" (aux-data-type obj))))

(defun aux-data-from-proto (proto)
  (let ((p-aux-data (proto:aux-data proto))
        (aux-data '()))
    (dotimes (n (length p-aux-data))
      (push (cons (pb:string-value (proto:key (aref p-aux-data n)))
                  (make-instance 'aux-data
                    :proto (proto:value (aref p-aux-data n))))
            aux-data))
    aux-data))

(defun aux-data-to-proto (aux-data)
  (map 'vector (lambda (pair)
                 (destructuring-bind (name . aux-data) pair
                   (let ((entry
                          (make-instance 'proto:module-aux-data-entry)))
                     (setf (proto:key entry) (pb:string-field name)
                           (proto:value entry) (proto aux-data))
                     entry)))
       aux-data))

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


;;;; Higher-level API functions.
(defgeneric get-block-by-uuid (uuid gtirb)
  ;; TODO: Maintain a hash of blocks by UUID on the IR level.
  (:documentation "Return the block keyed by UUID in GTIRB."))

(defmethod (setf get-block-by-uuid) (new (uuid simple-array) (obj gtirb))
  (setf (get-block-by-uuid (uuid-to-integer uuid) obj) new))

(defmethod (setf get-block-by-uuid) (new (uuid integer) (obj gtirb))
  (setf (gethash uuid (blocks obj)) new))

(defgeneric get-blocks-by-address (address gtirb)
  ;; TODO: Maintain a quick lookup of block by address on the IR level.
  (:documentation "Return the blocks located at ADDRESS in GTIRB."))
