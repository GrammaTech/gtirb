Common Lisp library for GTIRB
=============================

The Common Lisp API for GrammaTech's IR for Binaries (GTIRB).  GTIRB
is a data structure designed to support the analysis and rewriting of
binary executables.  There are a number of tools that produce, process
and consume GTIRB.  See the following for more information on GTIRB:
- [https://github.com/grammatech/gtirb](https://github.com/grammatech/gtirb) the main GTIRB source repository.
- [https://grammatech.github.io/gtirb/](https://grammatech.github.io/gtirb/) the GTIRB manual repository.
- [https://arxiv.org/abs/1907.02859](https://arxiv.org/abs/1907.02859) a white-paper describing the design goals of GTIRB.
- [https://github.com/grammatech/ddisasm](https://github.com/grammatech/ddisasm) a very high performance reassembleable disassembler producing GTIRB.
- [https://github.com/grammatech/gtirb-pprinter](https://github.com/grammatech/gtirb-pprinter) a pretty printer from GTIRB to assembler.

## Requirements and Installation
Hopefully, eventually, it will be possible to install everything by
(1) installing
[Protobuf](https://developers.google.com/protocol-buffers/), version
3.0.0 or later, and then (2) installing this Common Lisp GTIRB library
with QuickLisp `(ql:quickload :gtirb)`.

We're a ways away from that currently.  So after you've installed
Protobuf, you should clone and install the Common Lisp `PROTOBUF`
package according to the instructions at
[https://github.com/brown/protobuf](https://github.com/brown/protobuf).
(*Note*: currently you need my patched version of this library at
[updates-enabling-build-and-install branch of
github/eschulte/protobuf](https://github.com/eschulte/protobuf/tree/updates-enabling-build-and-install).
An upstream merge request is pending.)

## Usage
The Common Lisp API attempts to provide access to the underlying GTIRB
data-structure described above in idiomatic common lisp.  The main
Protobuf data structures are wrapped in CLOS objects.  All fields are
modifiable with `setf`.  Invariant are maintained automatically by the
API, e.g. using `:around` methods.

In some cases accessors are provided beyond the fields directly
present in the Protobuf.  For example, every GTIRB element has a UUID
(which supports referencing elements from AuxData tables).  The Common
Lisp API provides uniform access to any element through the `get-uuid`
method which operates similarly to `gethash` only it may be called on
any top-level GTIRB `IR` object (which itself maintains a hash of
every contained element by UUID).

The GTIRB CFG is represented as a graph using the Common Lisp graph
library from
[https://github.com/eschulte/graph](https://github.com/eschulte/graph).
This simple representation should promote easy exploration and
modification of the control flow graph, and the many graph analysis
functions defined in that library may be directly applied to the CFG.
Every node of the graph holds the UUID for a code block.

The bytes of any code and data block may be accessed by calling the
`bytes` method, which provides directly access to the bytes of the
block's `byte-interval`.

### Example Usage
See the test suite for a large number of basic usage examples.
However, the following gives a simple usage example.

1.  From the command-line.  Use the datalog disassembler `ddisasm` to
    disassemble the `ls` executable into a GTIRB instance.

        ddisasm --ir $(which ls) /tmp/ls.gtirb

2.  From the Common Lisp REPL.  Load the GTIRB API, and then load the
    GTIRB instance created in step (1) into a common lisp GTIRB object.

        (ql:quickload :gtirb)
        (use-package :gtirb)
        (defparameter ls (read-gtirb "/tmp/ls.gtirb"))

3.  At this point you can explore the CFG, perform analyses, or even
    modify the contents of the GTIRB object.  Results of analyses may
    be saved into new AuxData tables which become part of the GTIRB
    object for later use by other sessions or by other tools
    potentially written in other languages.

        ;; Do stuff with the GTIRB, maybe make changes.

4.  Finally, the resulting GTIRB object may be written back to the
    file system.

        (write-gtirb ls "/tmp/ls-modified.gtirb")

5.  At the command line.  A new executable may be created from the
    modified gtirb file using the `gtirb-pprinter`.

        gtirb-pprinter --ir /tmp/ls-modified.gtirb --binary /tmp/ls-modified
