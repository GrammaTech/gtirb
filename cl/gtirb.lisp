(defpackage :gtirb/gtirb
  (:nicknames :gtirb)
  (:use :common-lisp :alexandria :graph :trivia
        :trivial-utf-8
        :gtirb/ranged
        :gtirb/utility
        :named-readtables :curry-compose-reader-macros)
  (:shadow :symbol)
  (:import-from :gtirb.proto)
  (:import-from :trivial-package-local-nicknames :add-package-local-nickname)
  (:import-from :uiop :nest)
  (:import-from :asdf/system :system-relative-pathname)
  (:import-from :cl-intbytes
                :int->octets
                :octets->int64
                :octets->uint64)
  (:export :gtirb-version
           :protobuf-version
           :read-gtirb
           :write-gtirb
           :is-equal-p
           :*is-equal-p-verbose-p*
           :gtirb-node
           :get-uuid
           :remove-uuid
           :at-address
           :on-address
           :address-range
           :uuid
           :update-proto
;;; Classes and fields.
           :gtirb
           :ir
           :cfg
           :version
           ;; Module
           :module
           :name
           :binary-path
           :isa
           :file-format
           :preferred-addr
           :rebase-delta
           :symbols
           :proxies
           :sections
           :aux-data
           :entry-point
           ;; Symbol
           :symbol
           :value
           :payload
           :at-end
           ;; Section
           :section
           :byte-intervals
           :flags
           ;; Byte-Interval
           :byte-interval
           :blocks
           :addressp
           :address
           :contents
           :size
           :truncate-contents
           :ignore
           ;; Symbolic expressions
           :symbolic-expressions
           :sym-addr-const
           :sym-stack-const
           :sym-addr-addr
           :scale
           :*preserve-symbolic-expressions*
           ;; Block
           :gtirb-block
           :gtirb-byte-block
           :code-block
           :data-block
           :decode-mode
           :bytes
           :offset
           ;; Edge-Label
           :edge-label
           :conditional
           :direct
           :edge-type
           ;; Aux-Data
           :aux-data-type
           :aux-data-data
           ;; gtirb
           :modules))
(in-package :gtirb/gtirb)
(in-readtable :curry-compose-reader-macros)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-package-local-nickname :proto :gtirb.proto)
  (defvar version.txt
    `#.(nest (let ((version-path
                    (system-relative-pathname "gtirb" "../version.txt"))))
             (with-open-file (in version-path))
             (loop for line = (read-line in nil :eof)
                until (eql line :eof)
                collect (let ((delim (position #\Space line)))
                          (cons (intern (subseq line 0 delim))
                                (parse-integer (subseq line (1+ delim)))))))))

(define-constant gtirb-version
    (format nil "~d.~d.~d"
            (cdr (assoc 'VERSION_MAJOR version.txt))
            (cdr (assoc 'VERSION_MINOR version.txt))
            (cdr (assoc 'VERSION_PATCH version.txt)))
  :test #'string=
  :documentation "GTIRB Version as a string of \"MAJOR.MINOR.PATCH\".")

(define-constant protobuf-version
    (cdr (assoc 'VERSION_PROTOBUF version.txt))
  :test #'=
  :documentation "GTIRB Protobuf Version as a non-negative integer.")

(defgeneric read-gtirb (source)
  (:documentation "Read a protobuf serialized GTIRB instance from SOURCE.")
  (:method ((path t))
    (make-instance 'gtirb :proto (read-proto 'proto:ir path))))

(defmethod read-gtirb :around (source)
  "Check the protobuf version."
  (let ((gtirb (call-next-method)))
    (unless (= protobuf-version (proto:version (proto gtirb)))
      (error "Protobuf version mismatch version ~a from ~a isn't expected ~a"
             (proto:version (proto gtirb)) source protobuf-version))
    gtirb))

(defun write-gtirb (gtirb path)
  "Write a GTIRB IR object to PATH."
  (update-proto gtirb)
  (write-proto (proto gtirb) path))


;;;; Class utilities.
(defvar *is-equal-p-verbose-p* nil
  "Compare equality verbosely in the `is-equal-p' function.
This may be useful to print contextual information when an equality
comparison fails for a large object with many nested objects.")

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
  "Return t if LEFT and RIGHT are equal.
Recursively descend into any sub-structure.  Custom recursive equality
predicates are defined for common Common Lisp data structures as well
as all GTIRB structures."
  (let ((*is-equal-p-verbose-output-buffer* nil))
    (let ((equalp (is-equal-p-internal left right)))
      (prog1 equalp
        (when (and (not equalp) *is-equal-p-verbose-p*)
          (format t "~{~S~%~}"
                  (subseq *is-equal-p-verbose-output-buffer*
                          0 *is-equal-p-verbose-output-length*)))))))

(defmethod is-equal-p-internal :around ((left t) (right t))
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

(defclass gtirb-node () ()
  (:documentation "Objects with a UUID contained in a GTIRB instance."))

(defclass proto-backed (gtirb-node) ()
  (:documentation "Objects which may be serialized to/from protobuf."))

(defgeneric uuid (object)
  (:documentation "Return the UUID for OBJECT as an integer.")
  (:method ((obj proto-backed)) (uuid-to-integer (proto:uuid (proto obj)))))

(defgeneric get-uuid (uuid object)
  (:documentation "Get the referent of UUID in OBJECT."))

(defgeneric remove-uuid (uuid object)
  (:documentation "Remove the entry for UUID from OBJECT."))

(defgeneric (setf get-uuid) (new uuid object)
  (:documentation "Register REFERENT behind UUID in OBJECT."))

(defgeneric insert-address (object item start-address &optional end-address)
  (:documentation
   "Insert ITEM into OBJECT between START-ADDRESS and END-ADDRESS."))

(defgeneric delete-address (object item start-address &optional end-address)
  (:documentation
   "Delete ITEM from OBJECT between START-ADDRESS and END-ADDRESS."))

(defgeneric at-address (object address)
  (:documentation
   "Find all objects in OBJECT starting at ADDRESS."))

(defgeneric on-address (object start-address &optional end-address)
  (:documentation
   "Find all objects in OBJECT between START-ADDRESS and END-ADDRESS."))

(defgeneric set-parent-uuid (new uuid object)
  (:documentation "Set UUID to NEW in OBJECT's parent."))

(defgeneric update-proto (proto-backed-object)
  (:documentation
   "Update and return the `proto' field of PROTO-BACKED-OBJECT.
This will ensure that any changes made to PROTO-BACKED-OBJECT outside
of its protocol buffer, e.g. any slots initialized using the
:from-proto option to `define-proto-backed-class', are synchronized
against the object's protocol buffer.")
  (:method ((proto-backed-object proto-backed))
    (proto proto-backed-object)))

(defgeneric address-range (proto-backed-object)
  (:documentation
   "Return any address range of the PROTO-BACKED-OBJECT GTIRB object."))

(defmacro define-proto-backed-class ((class proto-class) super-classes
                                     slot-specifiers proto-fields
                                     &rest options)
  "Define a Common Lisp class backed by a protobuf class.
SLOT-SPECIFIERS is as in `defclass' with the addition of optional
:to-proto and :from-proto fields, which may take protobuf
serialization functions, and :skip-equal-p field which causes
`is-equal-p' to skip that field.  PROTO-FIELDS may hold a list of
fields which pass through directly to the backing protobuf class.  The
:parent option names the field holding the containing protobuf
element.  The :address-range option holds the logic to calculate an
address range for instances of the object."
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
         (parent (second (assoc :parent options)))
         (address-range (cdr (assoc :address-range options)))))
   `(progn
      (defclass ,class (proto-backed ,@super-classes)
        ;; Accessors for normal lisp classes
        ((proto :initarg :proto :accessor proto :type ,proto-class
                :initform ,(if parent
                               `(let ((it (make-instance ',proto-class)))
                                  (setf (proto::uuid it) (new-uuid))
                                  it)
                               `(make-instance ',proto-class))
                :documentation "Backing protobuf object.
Should not need to be manipulated by client code.")
         (ir :accessor ir :type (or null gtirb)
             :initarg :ir
             :initform nil
             :documentation
             ,(format nil "Access the top-level IR of this ~a." class))
         ;; TODO: Consider throwing warnings in a `setf :around'
         ;;       defmethod on the parents of objects with parents if
         ;;       the objects are set to something that already has a
         ;;       current parent.  This could avoid surprising
         ;;       inconsistencies.  Alternately this could throw an
         ;;       error with the option to copy the object with a new
         ;;       corrected parent or to set the parent directly.
         ,@(when parent
             `((,parent :accessor ,parent :type (or null ,parent)
                        :initarg ,(make-keyword parent)
                        :initform nil
                        :documentation ,(format nil "Access the ~a of this ~a."
                                                parent class))))
         ,@(mapcar [{plist-drop :to-proto} {plist-drop :from-proto}
                    {plist-drop :proto-field} {plist-drop :skip-equal-p}]
                   slot-specifiers))
        ,@(remove-if [«or {eql :parent} {eql :address-range}» #'car] options))
      ,@(when parent
          `((defmethod get-uuid (uuid (object ,class))
              (assert (or (ir object) (,parent object)) (object)
                      ,(format nil
                               "`get-uuid' failed on a ~a without a ~a"
                               class parent))
              (get-uuid uuid (or (ir object) (,parent object))))
            (defmethod set-parent-uuid (new uuid (object ,class))
              (assert (or (ir object) (,parent object)) (object)
                      ,(format nil
                               "`set-parent-uuid' failed on a ~a without a ~a"
                               class parent))
              (setf (get-uuid uuid (or (ir object) (,parent object))) new))
            (defmethod (setf get-uuid) (new uuid (object ,class))
              (assert (or (ir object) (,parent object)) (object)
                      ,(format nil
                               "`get-uuid' failed on ~a without a ~a"
                               class parent))
              (set-parent-uuid new uuid object))
            (defmethod remove-uuid (uuid (object ,class))
              (assert (or (ir object) (,parent object)) (object)
                      ,(format nil
                               "`remove-uuid' failed on ~a without a ~a"
                               class parent))
              (remove-uuid uuid (or (ir object) (,parent object))))
            (defmethod at-address ((object ,class) address)
              (assert (or (ir object) (,parent object)) (object)
                      ,(format nil
                               "`at-address' failed on ~a without a ~a"
                               class parent))
              (at-address (or (ir object) (,parent object)) address))
            (defmethod on-address ((object ,class) start &optional end)
              (assert (or (ir object) (,parent object)) (object)
                      ,(format nil
                               "`on-address' failed on ~a without a ~a"
                               class parent))
              (on-address (or (ir object) (,parent object)) start end))))
      (defmethod address-range ((self ,class)) ,@address-range)
      (defmethod
          initialize-instance :after ((self ,class) &key)
          ,@(when parent
              `((when (,parent self)
                  (setf (get-uuid (uuid-to-integer (proto:uuid (proto self)))
                                  self)
                        self))))
          (with-slots (proto ,@(mapcar #'car from-proto-slots)) self
            ,@(mapcar
               (lambda (spec)
                 (destructuring-bind
                       (slot &key from-proto &allow-other-keys) spec
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
                  (bytes `(setf ,base (force-byte-array new)))
                  (enumeration `(setf ,base (car (rassoc new ,enumeration))))
                  (uuid `(setf ,base (integer-to-uuid new)))
                  (string `(setf ,base (pb:string-field new)))))))
          proto-fields)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar aux-data-slot-definition
    "A-list of auxiliary data objects keyed by string name.
Aux-Data tables may hold structured or unstructured data.  This data
may refer to elements of the GTIRB IR through uuids.  Information
relevant to a particular module will be stored in Aux-Data tables
accessible from the specific module.  Aux-Data tables only exist on
modules and on GTIRB IR instances."))


;;;; Classes.
(define-proto-backed-class (gtirb proto:ir) ()
    ((modules :initarg modules :accessor modules :type list
              :initform nil
              :from-proto
              [{mapcar {make-instance 'module :ir self :gtirb self :proto}}
               {coerce _ 'list} #'proto:modules]
              :to-proto {map 'vector #'update-proto}
              :documentation
              "List of the modules on a top-level GTIRB IR instance.")
     (cfg :accessor cfg :type digraph
          :from-proto
          (lambda (proto)
            (let ((p-cfg (proto:cfg proto)))
              (populate
               (make-instance 'digraph)
               :edges-w-values
               (mapcar
                (lambda (edge)
                  (cons (list (uuid-to-integer (proto:source-uuid edge))
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
                    (destructuring-bind ((source target) . label) edge
                      (let ((p-edge (make-instance 'proto:edge)))
                        (setf
                         (proto:source-uuid p-edge) (integer-to-uuid source)
                         (proto:target-uuid p-edge) (integer-to-uuid target)
                         (proto:label p-edge) (proto label))
                        p-edge)))
                  (edges-w-values cfg)))
            p-cfg)
          :documentation
          "Control flow graph (CFG) represented as a `graph:digraph'.
Nodes in the graph hold the UUIDs of code blocks which may be looked
up using `get-uuid'.  Edges on the graph are labeled with `edge-label'
objects which provide information on the nature of the control flow of
the graph.")
     (aux-data :accessor aux-data :type list
               :from-proto #'aux-data-from-proto
               :to-proto #'aux-data-to-proto
               :documentation #.aux-data-slot-definition)
     (by-uuid :accessor by-uuid :initform (make-hash-table) :type hash-table
              :skip-equal-p t
              :documentation "Internal cache for UUID-based lookup.")
     (by-address :accessor by-address :initform (make-ranged) :skip-equal-p t
                 :documentation "Internal cache for Address-based lookup."))
    ((version :type unsigned-byte-64 :documentation "Protobuf version."))
  (:documentation "Base class of an instance of GTIRB IR."))

(define-condition ir (error)
  ((message :initarg :message :initform nil :reader message)
   (object :initarg :object :initform nil :reader object))
  (:report (lambda (condition stream)
             (format stream "GTIRB error ~S on ~S."
                     (message condition) (object condition))))
  (:documentation "Condition raised on GTIRB data structure violations."))

(defmethod print-object ((obj gtirb) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a" (modules obj))))

(defmethod ir ((obj gtirb)) obj)

(defmethod get-uuid (uuid (obj gtirb))
  (gethash uuid (by-uuid obj)))

(defmethod (setf get-uuid) (new uuid (obj gtirb))
  (when (zerop uuid)
    (warn "Saving object ~a without a UUID into ~a." new obj))
  (when-let ((range (address-range new)))
    #+debug (format t "(range ~S) ;; => ~S~%" new range)
    (apply #'insert-address obj new range))
  (setf (gethash uuid (by-uuid obj)) new))

(defmethod remove-uuid (uuid (obj gtirb))
  (remhash uuid (by-uuid obj)))

(defmethod insert-address ((gtirb gtirb) item start &optional end)
  (ranged-insert (by-address gtirb) (uuid item) start end))

(defmethod delete-address ((gtirb gtirb) item start &optional end)
  (ranged-delete (by-address gtirb) (uuid item) start end))

(defmethod at-address ((gtirb gtirb) address)
  (mapcar {get-uuid _ gtirb} (ranged-find-at (by-address gtirb) address)))

(defmethod on-address ((gtirb gtirb) start &optional (end start))
  (mapcar {get-uuid _ gtirb} (ranged-find (by-address gtirb) start end)))

(define-constant +module-isa-map+
    '((#.proto:+isa-isa-undefined+ . :undefined)
      (#.proto:+isa-ia32+ . :ia32)
      (#.proto:+isa-ppc32+ . :ppc32)
      (#.proto:+isa-x64+ . :x64)
      (#.proto:+isa-arm+ . :arm)
      (#.proto:+isa-valid-but-unsupported+ . :valid-but-unsupported)
      (#.proto:+isa-ppc64+ . :ppc64)
      (#.proto:+isa-arm64+ . :arm64)
      (#.proto:+isa-mips32+ . :mips32)
      (#.proto:+isa-mips64+ . :mips64))
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
    ((proxies :accessor proxies :type hash-table
              :initform (make-hash-table)
              :from-proto
              (lambda (proto &aux (table (make-hash-table)))
                (let ((proto-proxies (proto:proxies proto)))
                  (dotimes (n (length proto-proxies) table)
                    (let ((it (aref proto-proxies n)))
                      (setf (gethash (uuid-to-integer (proto:uuid it)) table)
                            (make-instance 'proxy-block
                              :ir (ir self) :module self :proto it))))))
              :to-proto
              [{map 'vector (lambda (uuid)
                              (let ((it (make-instance 'proto:proxy-block)))
                                (setf (proto:uuid it) (integer-to-uuid uuid))
                                it))}
               {mapcar #'car} #'hash-table-alist]
              :documentation
              "Hash-table of proxy-blocks keyed by UUID.
Proxy-blocks in GTIRB are used to represent cross-module linkages.
For example when code in a module calls to a function defined in an
external library, the CFG for that IR instance may represent this call
with a call edge to a proxy block representing the external called
function.")
     (symbols :accessor symbols :type list
              :initform nil
              :from-proto
              [{map 'list
                    {make-instance 'symbol :ir (ir self) :module self :proto}}
               #'proto:symbols]
              :to-proto {map 'vector #'update-proto}
              :documentation "Hash-table of symbols keyed by UUID.")
     (sections :accessor sections :type list
               :from-proto
               [{map 'list
                     {make-instance 'section :ir (ir self) :module self :proto}}
                #'proto:sections]
               :to-proto {map 'vector #'update-proto}
               :documentation "List of the sections comprising this module.")
     (aux-data :accessor aux-data :type list
               :from-proto #'aux-data-from-proto
               :to-proto #'aux-data-to-proto
               :documentation #.aux-data-slot-definition))
    ((name :type string :documentation
           "An optional human-readable name for this module.")
     (binary-path :type string :documentation
                  "The path or filename for this module.
E.g, the name of a dynamically loaded library or of the main
executable.")
     (preferred-addr :type unsigned-byte-64 :documentation
                     "Some systems specify a preferred address in memory.
On those systems this field may be used to capture this address.")
     (rebase-delta :type unsigned-byte-64 :documentation
                   "The difference between this module's and
`preferred-addr' and the address at which it was actually loaded.")
     (isa :type enumeration :enumeration +module-isa-map+ :documentation
          "The instruction set architecture (ISA) of the code in this module.")
     (file-format :type enumeration :enumeration +module-file-format-map+
                  :documentation
                  "The binary file format of the original file this
module represents."))
  (:documentation "Module of a GTIRB IR instance.") (:parent gtirb))

(defmethod print-object ((obj module) stream)
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

(define-proto-backed-class (edge-label proto:edge-label) () ()
    ((conditional :type boolean :documentation
                  "This is true if this edge is due to a conditional
instruction.")
     (direct :type boolean :documentation
             "Is this a direct (as opposed to indirect) control flow edge.")
     (edge-type :type enumeration :enumeration +edge-label-type-map+
                :proto-field type :documentation
                "The type of an edge indicates the nature of the
control flow along it.  E.g., \"branch,\" \"call,\" \"fallthrough,\"
and \"return\" are examples."))
  (:documentation "Label on a CFG edge.
This indicates the type of control flow along this edge."))

(defmethod print-object ((obj edge-label) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a ~:[unconditional~;conditional~] ~:[undirect~;direct~]"
            (edge-type obj) (conditional obj) (direct obj))))

(define-proto-backed-class (symbol proto:symbol) () ()
    ((name :type string)
     (value :type unsigned-byte-64)
     (referent-uuid :type uuid)
     (at-end :type boolean))
  (:documentation
   "Symbol with it's NAME and an optional VALUE or REFERENT.")
  (:parent module))

(defgeneric payload (symbol)
  (:documentation "Provide access to the referent or value of SYMBOL.")
  (:method ((symbol symbol))
    (cond
      ((proto:has-value (proto symbol))
       (value symbol))
      ((proto:has-referent-uuid (proto symbol))
       (get-uuid (referent-uuid symbol) symbol)))))

(defmethod (setf payload) ((new proto-backed) (symbol symbol))
  "Save GTIRB object NEW into the `referent-uuid' of SYMBOL."
  (proto:clear-value (proto symbol))
  (setf (referent-uuid symbol) (uuid new)))

(defmethod (setf payload) ((new integer) (symbol symbol))
  "Save INTEGER value NEW into the `value' of SYMBOL."
  (proto:clear-referent-uuid (proto symbol))
  (setf (value symbol) new))

(defmethod (setf payload) ((new t) (symbol symbol))
  (error "Symbol payload ~S must be either a GTIRB element or an integer." new))

(defmethod print-object ((obj symbol) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a ~a~:[~;|~]" (name obj)
            (or (value obj) (referent-uuid obj))
            (at-end obj))))

(define-constant +section-flags-map+
    '((#.proto:+section-flag-section-undefined+ . :flag-undefined)
      (#.proto:+section-flag-readable+ . :readable)
      (#.proto:+section-flag-writable+ . :writable)
      (#.proto:+section-flag-executable+ . :executable)
      (#.proto:+section-flag-loaded+ . :loaded)
      (#.proto:+section-flag-initialized+ . :initialized)
      (#.proto:+section-flag-thread-local+ . :thread-local))
  :test #'equal)

(define-proto-backed-class (section proto:section) ()
    ((byte-intervals
      :accessor byte-intervals :type list
      :from-proto
      [{map 'list
            {make-instance 'byte-interval :ir (ir self) :section self :proto}}
       #'proto:byte-intervals]
      :to-proto {map 'vector #'update-proto}
      :documentation "Byte-intervals holding all of the section's bytes."))
    ((name :type string :documentation "Name of this section.")
     (flags :type enumeration :enumeration +section-flags-map+
            :proto-field section-flags :documentation
            "Flags holding common properties of this section.
These flags only hold those section properties which are relatively
universal including read, write, execute permissions, whether the
section is loaded into memory at run-time or not, whether the section
is zero initialized, and whether the section is thread-local."))
  (:documentation "Section in a GTIRB IR instance.") (:parent module))

(defmethod print-object ((obj section) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a ~a" (name obj) (length (byte-intervals obj)))))

(defmethod address ((obj section))
  (extremum (mapcar #'address (byte-intervals obj)) #'<))

(defmethod size ((it section))
  (- (extremum (mapcar «+ #'address #'size» (byte-intervals it)) #'> :key #'car)
     (address it)))

(defgeneric blocks (obj)
  (:documentation "List of gtirb-byte-block objects in this object.
Primitive accessor for byte-interval.")
  (:method ((obj gtirb)) (mappend #'blocks (modules obj)))
  (:method ((obj module)) (mappend #'blocks (sections obj)))
  (:method ((obj section)) (mappend #'blocks (byte-intervals obj))))

(define-constant +se-attribute-flag-map+
    '((#.proto:+se-attribute-flag-part0+ . :part0)
      (#.proto:+se-attribute-flag-part1+ . :part1)
      (#.proto:+se-attribute-flag-part2+ . :part2)
      (#.proto:+se-attribute-flag-part3+ . :part3)
      (#.proto:+se-attribute-flag-adjusted+ . :adjusted)
      (#.proto:+se-attribute-flag-got+ . :got)
      (#.proto:+se-attribute-flag-got-rel-pc+ . :got-rel-pc)
      (#.proto:+se-attribute-flag-got-rel-got+ . :got-rel-got)
      (#.proto:+se-attribute-flag-addr-rel-got+ . :addr-rel-got)
      (#.proto:+se-attribute-flag-got-rel-addr+ . :got-rel-addr)
      (#.proto:+se-attribute-flag-got-page+ . :got-page)
      (#.proto:+se-attribute-flag-got-page-ofst+ . :got-page-ofst)
      (#.proto:+se-attribute-flag-plt-call+ . :plt-call)
      (#.proto:+se-attribute-flag-plt-ref+ . :plt-ref))
  :test #'equal)

(define-proto-backed-class (byte-interval proto:byte-interval) ()
    ((blocks :initarg :blocks :accessor blocks :type list
             :from-proto
             [{map 'list
                   (lambda (proto-block)
                     (let ((it (cond
                                 ((not (emptyp
                                        (proto:uuid (proto:data proto-block))))
                                  (make-instance 'data-block
                                    :ir (ir self)
                                    :byte-interval self
                                    :offset (proto:offset proto-block)
                                    :proto (proto:data proto-block)))
                                 ((not (emptyp
                                        (proto:uuid (proto:code proto-block))))
                                  (make-instance 'code-block
                                    :ir (ir self)
                                    :byte-interval self
                                    :offset (proto:offset proto-block)
                                    :proto (proto:code proto-block))))))
                       #+debug
                       (when (emptyp (proto:uuid (proto it)))
                         (warn "BAD BLOCK ~a with empty uuid from ~a.~%~A~%"
                               it (name (section self)) proto-block))
                       it))}
              #'proto:blocks]
             :to-proto
             {map 'vector
                  (lambda (gtirb-block)
                    (let ((it (make-instance 'proto:block)))
                      (setf (proto:offset it) (offset gtirb-block))
                      (etypecase gtirb-block
                        (code-block
                         (setf (proto:code it) (update-proto gtirb-block)))
                        (data-block
                         (setf (proto:data it) (update-proto gtirb-block))))
                      it))}
             :documentation
             "Blocks in this byte-interval.
This list could include `code-block' or `data-block' elements (which
both subclass the `gtirb-byte-block' class) but not `proxy-block'
elements as proxy blocks do not hold bytes.")
     (symbolic-expressions
      :accessor symbolic-expressions :type hash-table
      :initarg :symbolic-expressions
      :from-proto
      (lambda (proto &aux (table (make-hash-table)))
        (flet ((process-symbols (&rest symbols)
                 (mappend (lambda (uuid)
                            (if-let ((sym (get-uuid (uuid-to-integer uuid)
                                                    (ir self))))
                              (list sym)
                              (unless (emptyp uuid)
                                (warn "Symbol UUID ~S not found" uuid))))
                          symbols)))
          (dotimes (n (length (proto:symbolic-expressions proto)) table)
            (let* ((proto (aref (proto:symbolic-expressions proto) n))
                   (offset (proto:key proto))
                   (attribute-flags
                    (map 'list [#'cdr {assoc _ +se-attribute-flag-map+}]
                         (proto:attribute-flags (proto:value proto))))
                   (symbolic-expression (proto:value proto)))
              (setf (gethash offset table)
                    (cond
                      ((proto:has-stack-const symbolic-expression)
                       (make-instance 'sym-stack-const
                         :ir (ir self)
                         :attribute-flags attribute-flags
                         :symbols (process-symbols
                                   (proto:symbol-uuid
                                    (proto:addr-const symbolic-expression)))
                         :proto (proto:stack-const symbolic-expression)))
                      ((proto:has-addr-const symbolic-expression)
                       (make-instance 'sym-addr-const
                         :ir (ir self)
                         :attribute-flags attribute-flags
                         :symbols (process-symbols
                                   (proto:symbol-uuid
                                    (proto:addr-const symbolic-expression)))
                         :proto (proto:addr-const symbolic-expression)))
                      ((proto:has-addr-addr symbolic-expression)
                       (make-instance 'sym-addr-addr
                         :ir (ir self)
                         :attribute-flags attribute-flags
                         :symbols (process-symbols
                                   (proto:symbol1-uuid
                                    (proto:addr-addr symbolic-expression))
                                   (proto:symbol2-uuid
                                    (proto:addr-addr symbolic-expression)))
                         :proto (proto:addr-addr symbolic-expression)))
                      (t (assert "Symbolic expression of unknown kind."))))))))
      :to-proto
      [{map 'vector
            (lambda (pair)
              (destructuring-bind (offset . symbolic-expression) pair
                (flet ((force-mod-14-array (array)
                         (declare (type (simple-array) array))
                         (make-array (length array) :element-type '(mod 14)
                                     :initial-contents array)))
                  (let ((it (make-instance
                                'proto:byte-interval-symbolic-expressions-entry)))
                    (setf (proto:key it) offset
                          (proto:value it)
                          (let ((it (make-instance 'proto:symbolic-expression)))
                            (setf (proto:attribute-flags it)
                                  (force-mod-14-array
                                   (map 'vector
                                        [#'car {rassoc _ +se-attribute-flag-map+}]
                                        (attribute-flags symbolic-expression))))
                            (etypecase symbolic-expression
                              (sym-stack-const
                               (setf (proto:stack-const it)
                                     (update-proto symbolic-expression)))
                              (sym-addr-const
                               (setf (proto:addr-const it)
                                     (update-proto symbolic-expression)))
                              (sym-addr-addr
                               (setf (proto:addr-addr it)
                                     (update-proto symbolic-expression))))
                            it))
                    it))))}
       #'hash-table-alist]
      :documentation "Hash of symbolic-expressions keyed by offset."))
    ((addressp :type boolean :proto-field has-address
               :documentation
               "Does this byte-interval have an address.")
     (address :type unsigned-byte-64
              :documentation
              "Optionally specify the address in memory at which this
~ byte-interval should start.  Byte-intervals without address could
exist anywhere in memory.")
     (size :type unsigned-byte-64 :documentation
           "The size of this byte-interval.
It is possible for the size of a byte-interval to be larger than the
number of bytes in the byte interval's `contents' if portions of the
byte-interval are not represented statically but are zero-initialized
at runtime.")
     (contents :type bytes :documentation
               "A vector holding the actual bytes of this byte interval."))
  (:documentation "Byte-interval in a GTIRB instance.") (:parent section)
  (:address-range (when (addressp self)
                    (list (address self) (+ (address self) (size self))))))

(defmethod (setf size) :before (new (obj byte-interval))
  (restart-case
      (when (> (length (contents obj)) new)
        (error (make-condition 'ir
                               :message "size smaller than contents"
                               :object obj)))
    (truncate-contents ()
      :report "Truncate the contents of the byte-interval to the new size."
      (setf (contents obj) (subseq (contents obj) 0 new)))
    (ignore ()
      :report "Ignore and leave the byte-interval in an inconsistent state.")))

(defmethod print-object ((obj byte-interval) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a ~a"
            (if (addressp obj) (address obj) "?")
            (size obj))))

(defmethod address ((obj byte-interval))
  (when (addressp obj) (proto:address (proto obj))))

(defclass symbolic-expression ()
  ((symbols :accessor symbols :initarg :symbols :initform nil :type list
            :documentation "Symbol(s) appearing in this symbolic expression.")
   (attribute-flags :accessor attribute-flags :initarg :attribute-flags
                    :initform nil :type list
                    :documentation "Attributes holding the relocation type.")))

(defmethod print-object ((obj symbolic-expression) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a ~{~a~^, ~}" (offset obj) (symbols obj))))

;;; TODO: If we get symbolic expressions for these, then add the
;;; following to each `symbolic-expression' class:
;;;
;;;     (:parent byte-interval)
;;;     (:address-range (when-let ((range (addressp (byte-interval self))))
;;;                       (+ (offset self) (first range))))
;;;
;;; and then go back up and add ":byte-interval self" to their
;;; `make-instance' calls in byte-interval.
(define-proto-backed-class
    (sym-stack-const proto:sym-stack-const) (symbolic-expression) ()
    ((offset :type unsigned-byte-64))
  (:address-range (when (addressp (byte-interval self))
                    (let ((address (+ (address (byte-interval self))
                                      (offset self))))
                      (list address address)))))

(define-proto-backed-class
    (sym-addr-const proto:sym-addr-const) (symbolic-expression) ()
    ((offset :type unsigned-byte-64))
  (:address-range (when (addressp (byte-interval self))
                    (let ((address (+ (address (byte-interval self))
                                      (offset self))))
                      (list address address)))))

(define-proto-backed-class
    (sym-addr-addr proto:sym-addr-addr) (symbolic-expression) ()
    ((offset :type unsigned-byte-64)
     (scale :type unsigned-byte-64))
  (:address-range (when (addressp (byte-interval self))
                    (let ((address (+ (address (byte-interval self))
                                      (offset self))))
                      (list address address)))))

(defmethod update-proto :before ((sym sym-stack-const))
  (setf (proto:symbol-uuid (proto sym))
        (integer-to-uuid (uuid (first (symbols sym))))))

(defmethod update-proto :before ((sym sym-addr-const))
  (setf (proto:symbol-uuid (proto sym))
        (integer-to-uuid (uuid (first (symbols sym))))))

(defmethod update-proto :before ((sym sym-addr-addr))
  (setf (proto:symbol1-uuid (proto sym))
        (integer-to-uuid (uuid (first (symbols sym))))
        (proto:symbol2-uuid (proto sym))
        (integer-to-uuid (uuid (second (symbols sym))))))

(defmethod update-proto :before ((sym sym-stack-const))
  (setf (proto:symbol-uuid (proto sym))
        (integer-to-uuid (uuid (first (symbols sym))))))

(defmethod update-proto :before ((sym sym-addr-const))
  (setf (proto:symbol-uuid (proto sym))
        (integer-to-uuid (uuid (first (symbols sym))))))

(defmethod update-proto :before ((sym sym-addr-addr))
  (setf (proto:symbol1-uuid (proto sym))
        (integer-to-uuid (uuid (first (symbols sym))))
        (proto:symbol2-uuid (proto sym))
        (integer-to-uuid (uuid (second (symbols sym))))))

(defclass gtirb-block () ())

(defclass gtirb-byte-block (gtirb-block) ()
  (:documentation "Super-class of the `code-block' and `data-block' classes.
This class abstracts over all GTIRB blocks which are able to hold bytes."))

(defmethod symbolic-expressions ((bb gtirb-byte-block))
  (nest
   (alist-hash-table)
   (remove-if-not [«and {< (offset bb)} {>= (+ (offset bb) (size bb))}» #'car])
   (hash-table-alist)
   (symbolic-expressions)
   (byte-interval bb)))

(defmethod address ((obj gtirb-byte-block))
  (when-let ((base-address (address (byte-interval obj))))
    (+ base-address (offset obj))))

(defgeneric bytes (object &optional start end)
  (:documentation "Return the bytes held by OBJECT.")
  (:method ((obj byte-interval) &optional (start 0) end)
    (if end
        (subseq (contents obj) start end)
        (subseq (contents obj) start)))
  (:method ((obj gtirb-byte-block) &optional (start 0) (end (size obj)))
    #+debug (format t "[~S] ~S:[~S:~S]<-[~S:~S]~%"
                    (proto:uuid (proto obj))
                    (name (section (byte-interval obj)))
                    (or (and (addressp (byte-interval obj))
                             (address (byte-interval obj)))
                        "?")
                    (size (byte-interval obj))
                    (offset obj) (size obj))
    (let* ((start (+ (offset obj) start))
           (end (+ (offset obj) end)))
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

(defun shift-subseq (sequence start end)
  "Return a copy of SEQUENCE bounded by START and END."
  (subseq sequence start end))

(defparameter *preserve-symbolic-expressions* nil
  "When true, (setf bytes) preserves symbolic expressions
intersecting the assigned part of the object.")

(define-setf-expander shift-subseq (sequence start end &environment env)
  "Update the subseq of SEQUENCE bounded by START and END."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion sequence env)
    (declare (ignorable newval setter))
    (let ((store (gensym)))
      (values
       dummies                          ; Temporary variables
       vals                             ; Value forms.
       (list store)                     ; Store variables.
       `(progn
          (cond
            ((zerop ,start)
             (setf ,getter
                   (concatenate 'vector ,store (subseq ,getter ,end))))
            ((= (length ,store) (- ,end ,start))
             (setf (subseq ,getter ,start ,end) ,store))
            ((>= ,end (length ,getter))
             (setf ,getter
                   (concatenate 'vector (subseq ,getter 0 ,start) ,store)))
            (t (setf ,getter (concatenate 'vector
                                          (subseq ,getter 0 ,start)
                                          ,store
                                          (subseq ,getter ,end)))))
          ,store)                       ; Storing form.
       `(shift-subseq ,getter)))))      ; Accessing form.

(define-setf-expander bytes (sequence &optional (start 0) (end nil end-p)
                             &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion sequence env)
    (declare (ignorable newval setter))
    (let ((store (gensym))
          (end (if end-p end `(length (bytes ,getter)))))
      (values
       dummies                          ; Temporary variables
       vals                             ; Value forms.
       (list store)                     ; Store variables.
       `(progn
          (etypecase ,getter
            (gtirb::byte-interval
             (setf (shift-subseq (contents ,getter) ,start ,end) ,store))
            (gtirb::gtirb-byte-block
             (with-slots (offset) ,getter
               (let ((original-offset offset))
                 (setf (shift-subseq (contents (byte-interval ,getter))
                                     (+ offset ,start) (+ offset ,end))
                       ,store)
                 (setf-bytes-after ,store (byte-interval ,getter)
                                   (+ offset ,start) (+ offset ,end))
                 ;; Ensure the offset for THIS block isn't pushed back change.
                 (setf offset original-offset)))))
          (setf-bytes-after ,store ,getter ,start ,end)
          ,store)                       ; Storing form.
       `(bytes ,getter)))))             ; Accessing form.

(defgeneric setf-bytes-after (new object &optional start end)
  (:documentation
   "Update the offsets into BYTE-INTERVAL due to saving NEW into START END.")
  (:method (new (byte-interval byte-interval)
            &optional (start 0) (end (size byte-interval)))
    (setf (size byte-interval)
          (+ start (length new) (- (size byte-interval) end)))
    (let ((difference (- (length new) (- end start))))
      ;; Symbolic expressions.
      (nest (setf (symbolic-expressions byte-interval))
            (alist-hash-table)
            (mappend (lambda (pair)
                       (destructuring-bind (offset . sym-expr) pair
                         (cond
                           ((< offset start) (list (cons offset sym-expr)))
                           ((>= offset end)
                            (list (cons (+ offset difference) sym-expr)))
                           ;; Clear symbolic expressions in the modified range,
                           ;; unless *preserve-symbolic-expressions* is true.
                           (t (if *preserve-symbolic-expressions* (list pair) nil))))))
            (hash-table-alist (symbolic-expressions byte-interval)))
      ;; Byte-Blocks.
      (mapc (lambda (bb)
              (with-slots (offset) bb
                (when (>= offset end)
                  (incf offset difference))))
            (blocks byte-interval)))
    new)
  (:method (new (bb gtirb-byte-block) &optional (start 0) (end (size bb)))
    ;; Update the size of the byte-block
    (setf (size bb) (+ start (length new) (- (size bb) end)))
    new))

(define-proto-backed-class (code-block proto:code-block) (gtirb-byte-block)
    ((offset :initarg :offset :accessor offset :type number
             :documentation
             "Offset into this block's bytes in the block's byte-interval."))
    ((size :type unsigned-byte-64
           :documentation "The length of the bytes held by this code block.")
     (decode-mode :type unsigned-byte-64 :documentation
                  "Only present on architecture with multiple decode-modes."))
  (:documentation "Code-block in a GTIRB IR instance.") (:parent byte-interval)
  (:address-range (when-let ((range (address-range (byte-interval self))))
                    (list (+ (offset self) (first range))
                          (+ (offset self) (size self) (first range))))))

(defmethod print-object ((obj code-block) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a ~a" (size obj) (decode-mode obj))))

(defgeneric entry-point (module)
  (:documentation "The code-block which is the entry point of MODULE.")
  (:method ((obj module))
    (get-uuid (uuid-to-integer (proto:entry-point (proto obj))) obj)))

(defmethod (setf entry-point) ((new code-block) (obj module))
  (proto:clear-entry-point (proto obj))
  (setf (proto:entry-point (proto obj)) (integer-to-uuid (uuid new))))

(define-proto-backed-class (data-block proto:data-block) (gtirb-byte-block)
    ((offset :initarg :offset :accessor offset :type number
             :documentation
             "Offset into this block's bytes in the block's byte-interval."))
    ((size :type unsigned-byte-64 :documentation
           "The length of the bytes held by this data block."))
  (:documentation "Data-block in a GTIRB IR instance.")
  (:parent byte-interval)
  (:address-range (when-let ((range (address-range (byte-interval self))))
                    (list (+ (offset self) (first range))
                          (+ (offset self) (size self) (first range))))))

(defmethod print-object ((obj data-block) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a" (size obj))))

(define-proto-backed-class (proxy-block proto:proxy-block) (gtirb-block) () ()
  (:documentation "Proxy-block in a GTIRB IR instance.")
  (:parent module))

(defmethod print-object ((obj proxy-block) stream)
  (print-unreadable-object (obj stream :type t :identity t)))


;;;; AuxData type and data handling.
(define-proto-backed-class (aux-data proto:aux-data) () () ())

(defmethod print-object ((obj aux-data) stream)
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

(defgeneric aux-data-type (aux-data)
  (:documentation "Access the structured type of AUX-DATA.")
  (:method ((obj aux-data))
    (first (aux-data-type-read
            (pb:string-value (proto:type-name (proto obj)))))))

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

(defgeneric aux-data-data (aux-data)
  (:documentation "Access the structured representation of AUX-DATAs data.")
  (:method ((obj aux-data))
    (aux-data-decode (aux-data-type obj) (proto:data (proto obj)))))

(defmethod (setf aux-data-data) (new (obj aux-data))
  (setf (proto:data (proto obj))
        (force-byte-array (aux-data-encode (aux-data-type obj) new))))

(declaim (special *decode-data*))
(defun decode (type)
  (flet ((advance (n) (setf *decode-data* (subseq *decode-data* n))))
    (declare (inline advance))
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
       (mapcar #'decode types)))))
(defun aux-data-decode (type data)
  (let ((*decode-data* data))
    (decode type)))

(defun encode (type data)
  (flet ((extend (it) (push it *decode-data*)))
    (declare (inline extend))
    (match type
      ((or :addr :uint64-t :int64-t)
       (extend (int->octets data 8)))
      (:uuid
       (extend (integer-to-uuid data)))
      (:offset
       (etypecase data
         (proto:offset
          (progn (encode :uuid (uuid-to-integer (proto:element-id data)))
                 (encode :uint64-t (proto:displacement data))))
         (list
          (progn (encode :uuid (first data))
                 (encode :uint64-t (second data))))))
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
             types data)))))
(defun aux-data-encode (type data)
  (let ((*decode-data* nil))
    (encode type data)
    (reduce {concatenate 'vector} (reverse *decode-data*))))
