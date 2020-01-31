# GTIRB

The GrammaTech Intermediate Representation for Binaries (GTIRB) is a format for representing executables, shared libraries, and object code. It is intended to facilitate programs performing binary disassembly, analysis, transformation, and pretty printing, seeking to encourage communication and interoperability between tools.

GTIRB portably encodes binaries from a range of other formats, such as [ELF](https://en.wikipedia.org/wiki/Executable_and_Linkable_Format), [PE](https://en.wikipedia.org/wiki/Portable_Executable), and [Mach-O](https://en.wikipedia.org/wiki/Mach-O), allowing conversion to and from these formats to GTIRB without loss of information. In addition, it encodes information above and beyond what these formats store; it stores control flow, reference information, and other analysis results, allowing binary analysis and rewriting tools to have full information about the binary without processing it prior. Finally, it allows user-extensible data to be attached to any part of the file, letting tools communicate with each other in a standard, in-file format.

The remainder of this file describes various aspects of GTIRB:
- [GTIRB](#gtirb)
- [Overview](#overview)
  - [Representing Binaries](#representing-binaries)
    - [Sections](#sections)
    - [Symbols](#symbols)
    - [Symbolic Expressions](#symbolic-expressions)
  - [Additional Features](#additional-features)
    - [Control Flow Graphs](#control-flow-graphs)
    - [Byte Intervals](#byte-intervals)
    - [Auxiliary Data](#auxiliary-data)
  - [Code Blocks and Instruction Semantics](#code-blocks-and-instruction-semantics)
- [Building](#building)
  - [Requirements](#requirements)
- [Usage](#usage)
  - [Using Serialized GTIRB Data](#using-serialized-gtirb-data)
  - [GTIRB API Implementations](#gtirb-api-implementations)

# Overview

GTIRB has the following structure. Solid lines denote containment, and dotted lines denote referencing:

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

How GTIRB uses the following entities are explained below.

## Representing Binaries

Other executable file formats differ in many ways, but they all tend to have a similar structure. The content bytes of the image are divided into sections, which contain the bytes consisting of the text, data, etc. along with information about where they will be located at runtime. To facilitate linking with shared libraries, they have a symbol table, which specifies a list of names of entities this file provides, or is provided by another file. To facilitate being a shared library, the file contains a relocation table, a minimal set of indicators to what bytes in the binary need changed when the sections are rebased, so the library can be moved into the same memory space as the executable needing the library.

GTIRB contains all this information in some form, plus other information useful to binary manipulation tools. In GTIRB, a single binary is encoded as a *module*. A GTIRB file may have multiple modules, enclosed in a single *IR*. GTIRB encodes the standard features of all binary formats in the following manner:

### Sections

Modules in GTIRB contain multiple *sections*. A section has a name, a set of properties, and a set of contents (stored as byte intervals; see below for details).

### Symbols

Rather than storing a symbol table as a section, GTIRB stores a set of *symbols* in every module. These symbols have a name, a set of properties, and a *referent*. A referent may be a integer, indicating that the symbol is a numeric constant or fixed address, or a *block*. A block may be one of:

* A *code block*, a series of executable instructions.
* A *data block*, a series of bytes that might be read from or written to.
* A *proxy block*, indicating that the symbol is undefined, meaning that the actual block will be provided by another module after linking.

### Symbolic Expressions

To encode relocations, GTIRB stores *symbolic expressions* in every section (or, more technically, as part of byte intervals; see below for details). These specify that certain bytes in the binary refer to the address of a block in some manner. This allows these bytes to be recalculated when addresses change in the binary.

GTIRB does not specify exactly how symbolic expressions are transformed into bytes. This depends on where the symbolic expression is located; inside a code block, it depends on what part of an instruction it is of, while inside a data block, it depends on the size of the data block.

There are currently two kinds of symbolic expressions:

* *SymAddrConst*: This symbolic expression's value is the address of the referent of a symbol, plus or minus a fixed offset.
* *SymAddrAddr*: This symbolic expression's value is the difference between two symbols, divided by a scale and plus an offset.

## Additional Features

To support binary analysis and rewriting, GTIRB encodes things above and beyond what standard file formats to, so tools need not recompute analyses. Not only do GTIRB files contain complete relocation information, they also include:

### Control Flow Graphs

Attached to the IR is an intermodule *control flow graph*, or *CFG*. This graph describes what code blocks can lead to other code blocks during execution; vertices are blocks, while edges are a possible route of control flow between blocks.

More specifically, vertices in the CFG are *CFG nodes*, which can be either code blocks or proxy blocks. If control flows to a proxy block, it can represent either a call of an external function (if the proxy block is associated with a symbol) or a jump to an unknown location (if not associated with a symbol). The latter is often seen when a block ends in an indirect jump.

Edges in this graph have labels that store information about the control flow. It has a type that indicates what causes the control flow, such as by branching, by calling, or by falling through from an adjacent code block. It also has flags to determine whether or not the control flow is conditional or indirect; what an edge is conditional or indirect on can be determined by inspecting the last instruction in the code block.

### Byte Intervals

As stated above, sections contain bytes. However, they are not stored directly as part of the section. Instead, these sections are subdivided into chunks of bytes called *byte intervals*. This serves two purposes:

* Indicate what blocks can be moved independently of each other. It is guaranteed that you can shuffle around two byte intervals in a section, and doing so will preserve the program's semantics.
* Support the generation of blocks with no original address. Byte intervals may have a fixed address, but they may also be unfixed, which indicates that the byte interval was generated by a binary rewriting tool or is freely movable to any address.

Two byte intervals in the same section may not overlap in addresses (although sections can overlap with each other in some cases, such as in object code). However, byte intervals contain code blocks, data blocks, and symbolic expressions, and of those three, the two kinds of blocks can overlap within the same byte interval. Blocks can overlap in a few cases, for example:

* Particularly clever code blocks in variable-width ISAs can overlap by having a branch go into the middle of a larger instruction.
* There can coexist two overlapping data blocks, one for an array and one for an element of that array.
* Self-modifying code produces a situation where code blocks and data blocks overlap.

### Auxiliary Data

To support the vast world of additional information that can be included with a binary, GTIRB has a mechanism to attach arbitrary additional data to GTIRB files. This system is known as *auxiliary data*, or *aux data* for short. Aux data can be attached to IRs or modules. Aux data is stored as a map from *table names* to *tables*. Tables can be:

* Basic data types: integers, strings, etc.
* Nodes. What is a node? Any GTIRB object with a unique identifier: IRs, modules, sections, byte intervals, blocks, and symbols.
* Lists and tuples of aux data.
* Maps from aux data to aux data.

The aux data system is flexible, and users can even add their own types of aux data. Some information users might want about GTIRB files are stored as aux data, such as:

* Information about functions, which are encoded in aux data as sets of code blocks.
* Mappings between PLT and GOT thunks and what external symbols they correspond to.
* What bytes at the end of code blocks are padding, and could be trimmed if need be.

These standard tables are all part of the *sanctioned aux data*, a set of tables with [well-defined formats and semantics](AuxData.md). However, you're free to add tables of any type and function.

## Code Blocks and Instruction Semantics

GTIRB provides a set of code blocks indicating sets of executable instructions, but it should be noted that GTIRB explicitly does NOT represent instructions or instruction semantics but does provide symbolic operand information and access to the bytes.  There are many existing tools for representing instruction semantics, for example:

* [BAP](https://github.com/BinaryAnalysisPlatform/bap)'s [BIL](https://github.com/BinaryAnalysisPlatform/bil/releases/download/v0.1/bil.pdf)
* [Angr](http://angr.io)'s [Vex](https://github.com/angr/pyvex)
* [Ghidra](https://www.nsa.gov/resources/everyone/ghidra/)'s P-code

GTIRB works with these or any other IL by storing instructions generally and efficiently as *raw machine-code bytes* and separately storing the symbolic and control flow information.  The popular [Capstone](https://www.capstone-engine.org)/[Keystone](https://www.keystone-engine.org) decoder/encoder provide an excellent option to read and write instructions from/to GTIRB's machine-code byte representation without committing to any particular semantic IL.  By supporting multiple ILs and separate storage of analysis results in auxiliary data tables, GTIRB enables collaboration between independent binary analysis and rewriting teams and tools.

# Building

GTIRB should successfully build in 64-bits with GCC, Clang, and Visual Studio compilers supporting at least C++17.  GTIRB uses CMake which must be installed with at least version 3.9.

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
   - Ubuntu 16 and earlier provide out of date versions; build from source on those versions.
- [Protobuf](https://developers.google.com/protocol-buffers/), version 3.0.0 or later.
  - Ubuntu 18 provides this version via the APT packages `libprotobuf-dev` and `protobuf-compiler`.
  - Ubuntu 16 and earlier provide out of date versions; build from source on those versions.

# Usage

GTIRB is designed to be serialized using [Google's protocol buffers](https://developers.google.com/protocol-buffers/) (i.e., [protobuf](https://github.com/google/protobuf/wiki)), enabling [easy and efficient use from any programming language](#using-serialized-gtirb-data).

GTIRB may also be used through a dedicated API implemented in multiple languages. The APIs provide efficient data structures suitable for use by binary analysis and rewriting applications; see [below](#gtirb-api-implementations) for details.

## Using Serialized GTIRB Data

The serialized [protobuf](https://github.com/google/protobuf/wiki) data produced by GTIRB allows for exploration and manipulation in the language of your choice. The [Google protocol buffers](https://developers.google.com/protocol-buffers/) homepage lists the languages in which protocol buffers can be used directly; users of other languages can convert the protobuf-formatted data to JSON format and then use the JSON data in their applications.

The `proto` directory in this repository contains the protocol buffer message type definitions for GTIRB. You can inspect these `.proto` files to determine the structure of the various GTIRB message types. The top-level message type is `IR`.

For more details, see [Using Serialized GTIRB Data](PROTOBUF.md).

## GTIRB API Implementations

The GTIRB API is currently available in C++, Python, and Common Lisp. For language-independent API information, see [GTIRB Components](doc/general/ComponentsIndex.md). For information about the different API implementations, see:

  - [C++ API](doc/cpp/README.md)
  - [Python API](python/README.md)
  - [Common Lisp API](cl/README.md)
