# GTIRB

The GrammaTech Intermediate Representation for Binaries (GTIRB) is a
machine code analysis and rewriting data structure.  It is intended to
facilitate the communication of binary IR between programs performing
binary disassembly, analysis, transformation, and pretty printing.
GTIRB is modeled on LLVM-IR, and seeks to serve a similar
functionality of encouraging communication and interoperability
between tools.

The remainder of this file describes various aspects of GTIRB:
- [Structure](#structure)
- [Building](#building)
- [Usage](#usage)

# Structure

GTIRB has the following structure.  Solid lines denote inheritance.
Dotted lines denote reference by UUID.

       +--IPCFG--Edges...........................
       |                 .                      .
       |         +-ProxyBlocks             +-CodeBlocks
       |         |                         |
    IR-+-Modules-+-Sections--ByteIntervals-+-DataBlocks
       |    |    |                         |
       |    |    +-Symbols.........        +-SymbolicExpressions
       +-AuxData                  .              .
        --------                  ................
        ID0|DATA0
        ID1|DATA1....*anything*
        ID2|DATA2

## IR

An instance of GTIRB may include multiple modules (`Module`) which
represent loadable objects such as executables or libraries, an
inter-procedural control flow graph (`IPCFG`), and Auxiliary Data tables
(`AuxData`) which can hold arbitrary analysis results in user-defined
formats which can easily reference other elements of the IR.  Each
module holds information such as symbols (`Symbol`) and sections which
themselves hold the actual bytes and data and code blocks of the
module.  The CFG consists of basic blocks (`Block`) and control flow
edges between these blocks.  Each data or code block references a
range of bytes in a byte interval (`ByteInterval`).  A section may
hold one large byte interval holding all blocks---if the relative
positions of blocks in that section are defined---or may hold one byte
interval per block---if the relative positions of blocks is not
defined, e.g. for the code blocks in the `.text` section during
program rewriting.  Each symbol holds a pointer to the block or datum
it references.


## Instructions

GTIRB explicitly does NOT represent instructions or instruction
semantics but does provide symbolic operand information and access to
the bytes.  There are many *intermediate languages* (IL)s for
representation of instruction semantics (e.g.,
[BAP](https://github.com/BinaryAnalysisPlatform/bap)'s
[BIL](https://github.com/BinaryAnalysisPlatform/bil/releases/download/v0.1/bil.pdf),
[Angr](http://angr.io)'s [Vex](https://github.com/angr/pyvex), or
[Ghidra](https://www.nsa.gov/resources/everyone/ghidra/)'s P-code).
GTIRB works with these or any other IL by storing instructions
generally and efficiently as *raw machine-code bytes* and separately
storing the symbolic and control flow information.  The popular
[Capstone](https://www.capstone-engine.org)/[Keystone](https://www.keystone-engine.org)
decoder/encoder provide an excellent option to read and write
instructions from/to GTIRB's machine-code byte representation without
committing to any particular semantic IL.  By supporting multiple ILs
and separate storage of analysis results in auxiliary data tables
GTIRB enables collaboration between independent binary analysis and
rewriting teams and tools.


## Auxiliary Data

GTIRB provides for the sharing of additional information,
e.g. analysis results, in the form of `AuxData` objects.  These can
store maps and vectors of basic GTIRB types in a portable way. The
[GTIRB manual](https://grammatech.github.io/gtirb/) describes the
structure for common types of auxiliary data such as function boundary
information, type information, or results of common analyses in
[Standard AuxData Schemata](https://grammatech.github.io/gtirb/md__aux_data.html).


## UUIDs

Every element of GTIRB---e.g., modules (`Module`), symbols (`Symbol`),
and blocks (`Block`)---has a universally unique identifier (UUID).
UUIDs allow both first-class IR components and AuxData tables to
reference elements of the IR.

Instructions and symbolic operands can be addressed by the class
`Offset` which encapsulates a UUID (that refers to the instruction's
block) and an offset.

# Building

GTIRB should successfully build in 64-bits with GCC, Clang, and Visual
Studio compilers supporting at least C++17.  GTIRB uses CMake which
must be installed with at least version 3.9.

The common build process looks like this:
```sh
mkdir build
cd build
# Note: You may wish to add some -D arguments to the next command. See below.
cmake <path/to/gtirb>
cmake --build .
# Run the test suite.
bin/TestGTIRB
```

## Requirements

To build and install GTIRB, the following requirements should be installed:

- [CMake](https://cmake.org/), version 3.9.0 or higher.
   - Ubuntu 18 provides this version via the APT package `cmake`.
   - Ubuntu 16 and earlier provide out of date versions; build from
     source on those versions.
- [Protobuf](https://developers.google.com/protocol-buffers/), version
  3.0.0 or later.
  - Ubuntu 18 provides this version via the APT packages
    `libprotobuf-dev` and `protobuf-compiler`.
  - Ubuntu 16 and earlier provide out of date versions; build from
    source on those versions.

# Usage

GTIRB is designed to be serialized using [Google's protocol
buffers](https://developers.google.com/protocol-buffers/) (i.e.,
[protobuf](https://github.com/google/protobuf/wiki)), enabling [easy
and efficient use from any programming
language](#using-serialized-gtirb-data).

GTIRB may also be used through a dedicated API implemented in multiple
languages. The APIs provide efficient data structures suitable for use
by binary analysis and rewriting applications; see
[below](#gtirb-api-implementations) for details.

## Using Serialized GTIRB Data

The serialized [protobuf](https://github.com/google/protobuf/wiki)
data produced by GTIRB allows for exploration and manipulation in the
language of your choice. The [Google protocol
buffers](https://developers.google.com/protocol-buffers/) homepage
lists the languages in which protocol buffers can be used directly;
users of other languages can convert the protobuf-formatted data to
JSON format and then use the JSON data in their applications.

The `proto` directory in this repository contains the protocol buffer
message type definitions for GTIRB. You can inspect these `.proto`
files to determine the structure of the various GTIRB message
types. The top-level message type is `IR`.

For more details, see [Using Serialized GTIRB Data](PROTOBUF.md).

## GTIRB API Implementations

The GTIRB API is currently available in C++, Python, and Common
Lisp. For language-independent API information, see [GTIRB
Components](doc/general/ComponentsIndex.md). For information about the
different API implementations, see:

  - [C++ API](doc/cpp/README.md)
  - [Python API](python/README.md)
  - [Common Lisp API](cl/README.md)
