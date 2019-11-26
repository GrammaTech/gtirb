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

(defun module-bytes-subseq (module start end &aux (results #()))
  (let ((regions (proto-v0:regions (proto-v0:byte-map
                                    (proto-v0::image-byte-map module)))))
    (dotimes (n (length regions) results)
      (let* ((region (aref regions n))
             (address (proto-v0:address region))
             (size (length (proto-v0:data region))))
        (cond
          ((and (<= address start) (<= start (+ address size)))
           (setf results (concatenate 'vector
                                      results
                                      (subseq (proto-v0:data region)
                                              (- start address)
                                              (min size (- end address)))))
           (setf start (min end (+ address size))))
          ((= start end) (return-from module-bytes-subseq results)))))))

(defun byte-interval (module section
                      &aux (new (make-instance 'proto:byte-interval)))
  (let ((address (proto-v0::address section))
        (size (proto-v0::size section)))
    (setf (proto::address new) address
          (proto::size new) size
          (proto::bytes new)
          (module-bytes-subseq module address (+ address size))
          (proto::symbolic-expressions new)
          (map 'vector {upgrade _ :base address}
               (remove-if-not [«and {<= address} {>= (+ address size)}» #'proto-v0:key]
                              (proto-v0:symbolic-operands module)))
          (proto::blocks new)
          (map 'vector (lambda (block)
                         (etypecase block
                           (proto-v0::block
                               (let ((it (make-instance 'proto::block-wrapper)))
                                 (setf (proto:code-block it)
                                       (upgrade block :base address))
                                 it))
                           (proto-v0::data-object
                            (let ((it (make-instance 'proto::block-wrapper)))
                              (setf (proto:data-block it)
                                    (upgrade block :base address))
                              it))))
               (remove-if-not
                (lambda (block)
                  (let ((addr (proto-v0::address block)))
                    (and (<= address addr)
                         (<= addr (+ address size)))))
                (concatenate 'vector
                             (proto-v0::blocks module)
                             (proto-v0::data module))))))
  (coerce (list new) 'vector))

(defun entry-point-block (module)
  (let ((address (proto-v0::entry-point-address
                  (proto-v0::image-byte-map module))))
    (proto-v0::uuid
     (find-if «and [{<= address} «+ #'proto-v0::address #'proto-v0::size»]
                   [{>= address} #'proto-v0::address]»
              (proto-v0::blocks module)))))

(defun transfer-fields (new old fields)
  (mapc (lambda (field)
          (setf (slot-value new (intern (symbol-name field) 'proto))
                (upgrade (funcall (intern (symbol-name field) 'proto-v0) old))))
        fields))

(defgeneric upgrade (object &key &allow-other-keys)
  (:documentation "Upgrade OBJECT to the next protobuf version.")
  (:method ((old t) &key &allow-other-keys) old)
  (:method ((old array) &key  &allow-other-keys)
    (if (every #'numberp old)
        (make-array (length old) :element-type '(unsigned-byte 8)
                    :initial-contents old)
        (map 'vector #'upgrade old)))
  (:method ((old proto-v0::ir) &key &allow-other-keys
            &aux (new (make-instance 'proto::ir)))
    (setf (proto::uuid new) (proto-v0::uuid old)
          (proto::version new) (pb:string-field "1.0.0")
          (proto::aux-data new) (upgrade (proto-v0::aux-data-container old)
                                         :new-class 'proto:ir-aux-data-entry)
          (proto::modules new) (upgrade (proto-v0::modules old)))
    new)
  (:method ((old proto-v0::module) &key &allow-other-keys
            &aux (new (make-instance 'proto::module)))
    (transfer-fields new old
                     '(uuid binary-path preferred-addr rebase-delta file-format
                       isa-id name symbols cfg proxies name))
    (setf (proto::aux-data new) (upgrade (proto-v0::aux-data-container old)
                                         :new-class 'proto:module-aux-data-entry)
          (proto::sections new) (map 'vector {upgrade _ :module old}
                                     (proto-v0::sections old))
          (proto:entry-point-block new) (entry-point-block old))
    new)
  (:method ((old proto-v0::aux-data-container) &key new-class &allow-other-keys)
    (map 'vector (lambda (entry)
                   (let ((it (make-instance new-class)))
                     (setf (proto:key it) (proto-v0:key entry)
                           (proto:value it) (upgrade (proto-v0:value entry)))
                     it))
         (proto-v0::aux-data old)))
  (:method ((old proto-v0::aux-data) &key module &allow-other-keys
            &aux (new (make-instance 'proto::aux-data)))
    (transfer-fields new old '(type-name data))
    new)
  (:method ((old proto-v0::section) &key module &allow-other-keys
            &aux (new (make-instance 'proto::section)))
    (transfer-fields new old '(uuid name))
    (setf (proto::byte-intervals new) (byte-interval module old))
    new)
  (:method ((old proto-v0:edge-label) &key &allow-other-keys
            &aux (new (make-instance 'proto:edge-label)))
    (transfer-fields new old '(conditional direct type))
    new)
  (:method ((old proto-v0:edge) &key &allow-other-keys
            &aux (new (make-instance 'proto:edge)))
    (transfer-fields new old '(source-uuid target-uuid label))
    new)
  (:method ((old proto-v0::cfg) &key &allow-other-keys
            &aux (new (make-instance 'proto:cfg)))
    (transfer-fields new old '(vertices edges))
    new)
  (:method ((old proto-v0::module-symbolic-operands-entry)
            &key base &allow-other-keys
            &aux (new (make-instance
                          'proto:byte-interval-symbolic-expressions-entry)))
    (setf (proto:key new) (- (proto-v0:key old) base)
          (proto:value new) (upgrade (proto-v0:value old))))
  (:method ((old proto-v0::symbolic-expression) &key &allow-other-keys
            &aux (new (make-instance 'proto:symbolic-expression)))
    (transfer-fields new old '(stack-const addr-const addr-addr))
    new)
  (:method ((old proto-v0::sym-stack-const) &key &allow-other-keys
            &aux (new (make-instance 'proto:sym-stack-const)))
    (transfer-fields new old '(offset symbol-uuid))
    new)
  (:method ((old proto-v0::sym-addr-const) &key &allow-other-keys
            &aux (new (make-instance 'proto:sym-addr-const)))
    (transfer-fields new old '(offset symbol-uuid))
    new)
  (:method ((old proto-v0::sym-addr-addr) &key &allow-other-keys
            &aux (new (make-instance 'proto:sym-addr-addr)))
    (transfer-fields new old '(scale offset symbol1-uuid symbol2-uuid))
    new)
  (:method ((old proto-v0::block) &key base &allow-other-keys
            &aux (new (make-instance 'proto:code-block)))
    (transfer-fields new old '(uuid size decode-mode))
    (setf (proto::offset new) (- (proto-v0:address old) base))
    new)
  (:method ((old proto-v0::data-object) &key base &allow-other-keys
            &aux (new (make-instance 'proto:data-block)))
    (transfer-fields new old '(uuid size))
    (setf (proto::offset new) (- (proto-v0:address old) base))
    new))

(define-command update (input-file output-file &spec +udpate-args+)
  "Update GTIRB protobuf from INPUT-FILE to OUTPUT-FILE." ""
  (when help (show-help-for-update) (sb-ext:quit))
  (write-proto (upgrade (read-proto 'proto-v0:ir input-file)) output-file))
