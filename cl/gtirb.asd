(defsystem "gtirb"
    :name "gtirb"
    :author "GrammaTech"
    :licence "MIT"
    :description "Common Lisp library for GTIRB"
    :long-description "A Common Lisp front end to the GrammaTech
  Intermediate Representation for Bianries (GTIRB).  GTIRB is
  serialized using Google's protocol buffers.  This library wraps the
  raw protocol buffer serialization with a more Lispy interface."
    :depends-on (:gtirb/gtirb)
    :class :package-inferred-system
    :defsystem-depends-on (:asdf-package-system)
    :in-order-to ((test-op (load-op "gtirb/test")))
    :perform (test-op (o c) (symbol-call :gtirb/test '#:test)))

(defsystem "proto-v0"
    :name "proto-v0"
    :description "Common Lisp interface to (old V0) GTIRB protobuf files"
    :author "GrammaTech"
    :license "MIT"
    :depends-on (:protobuf)
    :components
    ((:static-file "README.md")
     ;; See the protobuf defsystem extension for how the gtirb.proto
     ;; file is loaded into Lisp.  https://github.com/brown/protobuf
     (:module proto
              :pathname "../proto/v0/"
              :components
              ((:file "AuxDataContainer")
               (:file "CFG")
               (:file "Section")
               (:file "Offset")
               (:file "IR")
               (:file "ByteMap")
               (:file "ProxyBlock")
               (:file "AuxData")
               (:file "Module")
               (:file "DataObject")
               (:file "ImageByteMap")
               (:file "SymbolicExpression")
               (:file "Symbol")
               (:file "Block")))))

(defsystem "proto"
    :name "proto"
    :description "Common Lisp interface to GTIRB protobuf files"
    :author "GrammaTech"
    :license "MIT"
    :depends-on (:protobuf)
    :components
    ((:static-file "README.md")
     ;; See the protobuf defsystem extension for how the gtirb.proto
     ;; file is loaded into Lisp.  https://github.com/brown/protobuf
     (:module proto
              :pathname "../proto"
              :components
              ((:file "AuxData")
               (:file "ByteInterval")
               (:file "CFG")
               (:file "CodeBlock")
               (:file "DataBlock")
               (:file "IR")
               (:file "Module")
               (:file "ProxyBlock")
               (:file "Section")
               (:file "Symbol")
               (:file "SymbolicExpression")))))

(defsystem "gtirb/run-update"
    :author "GrammaTech"
    :licence "MIT"
    :description "Convert between GTIRB protobuf versions."
    :depends-on (gtirb/update)
    :build-operation "asdf:program-op"
    :build-pathname "gtirb-update"
    :entry-point "gtirb/update::run-update")

(defsystem "gtirb/run-dot"
    :author "GrammaTech"
    :licence "MIT"
    :description "Write GTIRB to a dot graph."
    :depends-on (gtirb/dot)
    :build-operation "asdf:program-op"
    :build-pathname "gtirb-dot"
    :entry-point "gtirb/dot::run-dot")

(register-system-packages "proto" '(:gtirb.proto))
(register-system-packages "protobuf" '(:protocol-buffer))
(register-system-packages "cl-interval" '(:interval))
