(defpackage :gtirb/update
  (:use :common-lisp
        :alexandria
        :gtirb/utility
        :named-readtables
        :curry-compose-reader-macros
        :command-line-arguments)
  (:import-from :proto)
  (:import-from :proto-v0)
  (:import-from :uiop/image :quit)
  (:export :update :upgrade :read-proto :write-proto))
(in-package :gtirb/update)
(in-readtable :curry-compose-reader-macros)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +udpate-args+
    '((("help" #\h #\?) :type boolean :optional t
       :documentation "display help output"))))

(defun module-bytes-subseq (module start end &aux (results #()))
  (let ((regions (proto-v0:regions (proto-v0:byte-map
                                    (proto-v0:image-byte-map module)))))
    (force-byte-array
     (dotimes (n (length regions) results)
       (let* ((region (aref regions n))
              (address (proto-v0:address region))
              (size (length (proto-v0:data region))))
         (cond
           ((and (<= address start) (< start (+ address size)))
            (setf results (concatenate 'vector
                                       results
                                       (subseq (proto-v0:data region)
                                               (- start address)
                                               (min size (- end address)))))
            (setf start (min end (+ address size))))
           ((= start end) (return results))))))))

(defun byte-interval (module section
                      &aux (new (make-instance 'proto:byte-interval)))
  (let ((address (proto-v0:address section))
        (size (proto-v0:size section)))
    (setf (proto:uuid new) (new-uuid)
          (proto:address new) address
          (proto:has-address new) (proto-v0:has-address section)
          (proto:size new) size
          (proto:contents new)
          (module-bytes-subseq module address (+ address size))
          (proto:symbolic-expressions new)
          (map 'vector {upgrade _ :base address}
               (remove-if-not [«and {<= address} {>= (+ address size)}» #'proto-v0:key]
                              (proto-v0:symbolic-operands module)))
          (proto:blocks new)
          (map 'vector (lambda (block)
                         (etypecase block
                           (proto-v0:block
                               (let ((it (make-instance 'proto:block)))
                                 (setf (proto:code it)
                                       (upgrade block)
                                       (proto:offset it)
                                       (- (proto-v0:address block) address))
                                 it))
                           (proto-v0:data-object
                            (let ((it (make-instance 'proto:block)))
                              (setf (proto:data it)
                                    (upgrade block :base address)
                                    (proto:offset it)
                                    (- (proto-v0:address block) address))
                              it))))
               (remove-if-not
                (lambda (block)
                  (let ((addr (proto-v0:address block)))
                    (and (<= address addr)
                         (< addr (+ address size)))))
                (concatenate 'vector
                             (proto-v0:blocks module)
                             (proto-v0:data module))))))
  (coerce (list new) 'vector))

(defun entry-point (module)
  (when-let ((address (proto-v0:entry-point-address
                       (proto-v0:image-byte-map module))))
    (if-let ((gtirb-block
              (find-if
               «and [{< address} «+ #'proto-v0:address #'proto-v0:size»]
                    [{>= address} #'proto-v0:address]»
               (proto-v0:blocks module))))
      (proto-v0:uuid gtirb-block)
      (if (zerop address)
          (warn "Zero address found in module ~S, assuming not an entry point."
                (pb:string-value (proto-v0:name module)))
          (error "No block found holding module ~S entry point ~a."
                 (pb:string-value (proto-v0:name module))
                 address)))))

(defmacro transfer-fields (new old &rest fields)
  `(progn
     ,@(mapcar (lambda (field)
                 `(setf (,(intern (symbol-name field) 'proto) ,new)
                        (upgrade (,(intern (symbol-name field) 'proto-v0) ,old))))
               fields)))

#+debug
(defun serial (it)
  "Useful to ensure yourself of what protobuf serialization is producing."
  (let* ((size (pb:octet-size it))
         (buffer (make-array size :element-type '(unsigned-byte 8))))
    (pb:serialize it buffer 0 size)
    buffer))

#+debug
(defun deserial (class bytes &aux (it (make-instance class)))
  "Useful to ensure yourself of what protobuf deserialization is producing."
  (pb:merge-from-array it bytes 0 (length bytes))
  it)

(defun combine-cfgs (cfgs)
  (make-instance 'proto:cfg
    :edges (coerce (mappend [{coerce _ 'list} #'proto:edges] cfgs) 'vector)
    :vertices (coerce (mappend [{coerce _ 'list} #'proto:vertices] cfgs) 'vector)))

(defgeneric upgrade (object &key &allow-other-keys)
  (:documentation "Upgrade OBJECT to the next protobuf version.")
  (:method ((old t) &key &allow-other-keys) old)
  (:method ((old array) &key  &allow-other-keys)
    (if (every #'numberp old)
        (force-byte-array old)
        (map 'vector #'upgrade old)))
  (:method ((old proto-v0:ir) &key &allow-other-keys
            &aux (new (make-instance 'proto:ir)))
    (setf (proto:uuid new) (proto-v0:uuid old)
          (proto:version new) 1
          (proto:aux-data new) (upgrade (proto-v0:aux-data-container old)
                                        :new-class 'proto:ir-aux-data-entry)
          (proto:modules new) (upgrade (proto-v0:modules old))
          (proto:cfg new) (combine-cfgs (map 'list [#'upgrade #'proto-v0:cfg]
                                             (proto-v0:modules old))))
    new)
  (:method ((old proto-v0:module) &key &allow-other-keys
            &aux (new (make-instance 'proto:module)))
    (transfer-fields new old
                     uuid binary-path preferred-addr rebase-delta file-format
                     name symbols proxies name)
    (setf (proto:isa new) (proto-v0:isa-id old)
          (proto:aux-data new) (upgrade (proto-v0:aux-data-container old)
                                        :new-class 'proto:module-aux-data-entry)
          (proto:sections new) (map 'vector {upgrade _ :module old}
                                    (proto-v0:sections old)))
    (if-let ((entry-point (entry-point old)))
      (setf (proto:entry-point new) entry-point))
    new)
  (:method ((old proto-v0:aux-data-container) &key new-class &allow-other-keys)
    (map 'vector (lambda (entry)
                   (let ((it (make-instance new-class)))
                     (setf (proto:key it) (proto-v0:key entry)
                           (proto:value it) (upgrade (proto-v0:value entry)))
                     it))
         (proto-v0:aux-data old)))
  (:method ((old proto-v0:aux-data) &key &allow-other-keys
            &aux (new (make-instance 'proto:aux-data)))
    (transfer-fields new old type-name data)
    new)
  (:method ((old proto-v0:section) &key module &allow-other-keys
            &aux (new (make-instance 'proto:section)))
    (transfer-fields new old uuid name)
    (setf (proto:byte-intervals new) (byte-interval module old))
    new)
  (:method ((old proto-v0:edge-label) &key &allow-other-keys
            &aux (new (make-instance 'proto:edge-label)))
    (transfer-fields new old conditional direct type)
    new)
  (:method ((old proto-v0:edge) &key &allow-other-keys
            &aux (new (make-instance 'proto:edge)))
    (transfer-fields new old source-uuid target-uuid label)
    new)
  (:method ((old proto-v0:cfg) &key &allow-other-keys
            &aux (new (make-instance 'proto:cfg)))
    (transfer-fields new old vertices edges)
    new)
  (:method ((old proto-v0:module-symbolic-operands-entry)
            &key base &allow-other-keys
            &aux (new (make-instance
                          'proto:byte-interval-symbolic-expressions-entry)))
    (setf (proto:key new) (- (proto-v0:key old) base)
          (proto:value new) (upgrade (proto-v0:value old)))
    new)
  (:method ((old proto-v0:symbol) &key &allow-other-keys
            &aux (new (make-instance 'proto:symbol)))
    ;; TODO: Populate an AuxData table for storage-kind.
    (transfer-fields new old uuid name)
    (cond                ; Variant "oneof" 'value' or 'referent_uuid'.
      ((proto-v0:has-value old)
       (setf (proto:value new) (proto-v0:value old)))
      ((proto-v0:has-referent-uuid old)
       (setf (proto:referent-uuid new) (upgrade (proto-v0:referent-uuid old)))))
    new)
  (:method ((old proto-v0:symbolic-expression) &key &allow-other-keys
            &aux (new (make-instance 'proto:symbolic-expression)))
    (cond                               ; Variant "oneof" field.
      ((proto-v0:has-stack-const old)
       (setf (proto:stack-const new) (upgrade (proto-v0:stack-const old))))
      ((proto-v0:has-addr-const old)
       (setf (proto:addr-const new) (upgrade (proto-v0:addr-const old))))
      ((proto-v0:has-addr-addr old)
       (setf (proto:addr-addr new) (upgrade (proto-v0:addr-addr old))))
      (t (warn "Symbolic expressions ~s has no value." old)))
    #+debug
    (progn     ; Potentially useful debug pattern to inspect protobuf.
      (format t "~%~%~%OLD:~S~%" (serial old))
      (describe old)
      (format t "~%NEW:~S~%" (serial new))
      (describe new))
    new)
  (:method ((old proto-v0:sym-stack-const) &key &allow-other-keys
            &aux (new (make-instance 'proto:sym-stack-const)))
    (transfer-fields new old offset symbol-uuid)
    new)
  (:method ((old proto-v0:sym-addr-const) &key &allow-other-keys
            &aux (new (make-instance 'proto:sym-addr-const)))
    (transfer-fields new old symbol-uuid)
    new)
  (:method ((old proto-v0:sym-addr-addr) &key &allow-other-keys
            &aux (new (make-instance 'proto:sym-addr-addr)))
    (transfer-fields new old scale offset symbol1-uuid symbol2-uuid)
    new)
  (:method ((old proto-v0:block) &key &allow-other-keys
            &aux (new (make-instance 'proto:code-block)))
    (transfer-fields new old uuid size decode-mode)
    new)
  (:method ((old proto-v0:data-object) &key &allow-other-keys
            &aux (new (make-instance 'proto:data-block)))
    (transfer-fields new old uuid size)
    new))

(define-command update (input-file output-file &spec +udpate-args+)
  "Update GTIRB protobuf from INPUT-FILE to OUTPUT-FILE." ""
  (when help (show-help-for-update) (quit))
  (setf *random-state* (make-random-state t))
  (write-proto (upgrade (read-proto 'proto-v0:ir input-file)) output-file))
