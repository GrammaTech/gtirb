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
- [Installing](#installing)
- [Building](#building)
- [Usage](#usage)

# Structure

GTIRB has the following structure.  Solid lines denote inheritance.
Dotted lines denote reference by UUID.

![GTIRB Data Structure](.gtirb.svg)

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
representation of instruction semantics (e.g., [BAP][]'s [BIL][],
[Angr][]'s [Vex][], or [Ghidra][]'s P-code).  GTIRB works with these
or any other IL by storing instructions generally and efficiently as
*raw machine-code bytes* and separately storing the symbolic and
control flow information.  The popular [Capstone][]/[Keystone][]
decoder/encoder provide an excellent option to read and write
instructions from/to GTIRB's machine-code byte representation without
committing to any particular semantic IL.  By supporting multiple ILs
and separate storage of analysis results in auxiliary data tables
GTIRB enables collaboration between independent binary analysis and
rewriting teams and tools.

[BAP]: https://github.com/BinaryAnalysisPlatform/bap
[BIL]: https://github.com/BinaryAnalysisPlatform/bil/releases/download/v0.1/bil.pdf
[Angr]: http://angr.io
[Vex]: https://github.com/angr/pyvex
[Ghidra]: https://www.nsa.gov/resources/everyone/ghidra/
[Capstone]: https://www.capstone-engine.org
[Keystone]: https://www.keystone-engine.org


## Auxiliary Data

GTIRB provides for the sharing of additional information,
e.g. analysis results, in the form of `AuxData` objects.  These can
store maps and vectors of basic GTIRB types in a portable way. The
[GTIRB manual][] describes the structure for common types of auxiliary
data such as function boundary information, type information, or
results of common analyses in [Standard AuxData Schemata][].

[GTIRB manual]: https://grammatech.github.io/gtirb/
[Standard AuxData Schemata]: https://grammatech.github.io/gtirb/md__aux_data.html


## UUIDs

Every element of GTIRB---e.g., modules (`Module`), symbols (`Symbol`),
and blocks (`Block`)---has a universally unique identifier (UUID).
UUIDs allow both first-class IR components and AuxData tables to
reference elements of the IR.

Instructions and symbolic operands can be addressed by the class
`Offset` which encapsulates a UUID (that refers to the instruction's
block) and an offset.


# Installing

Packages currently exist for easily installing GTIRB (and attendant
tooling including the [ddisasm][] disassembler and [gtirb-pprinter][]
pretty printer) on Windows, and Ubuntu 20. See below for
instructions. Additionally, a public Docker image exists at
[grammatech/ddisasm][] with all of these tools installed. GTIRB is
versioned with Major.Minor.Patch versioning where Major version
increments will require significant source changes but should be very
rare, Minor version increments may require small source changes, and
Patch version increments shouldn't break any downstream builds. We do
not yet provide ABI compatibility across any version changes.

[ddisasm]: https://github.com/GrammaTech/ddisasm
[gtirb-pprinter]: https://github.com/GrammaTech/gtirb-pprinter
[grammatech/ddisasm]: https://hub.docker.com/r/grammatech/ddisasm


## Python API

The latest stable GTIRB Python API may be installed from PyPI using pip:

```sh
pip install gtirb
```

The latest unstable version of the Python API can be installed from a
prebuilt wheel:

```sh
pip install https://download.grammatech.com/gtirb/files/python/gtirb-unstable-py3-none-any.whl
```

It is critical that the choice of a `stable` or `unstable` package matches the
installed ddisasm and gtirb-pprinter packages.

## Windows

Windows releases are packaged as .zip files and are available at
https://download.grammatech.com/gtirb/files/windows-release/.

## Ubuntu

Packages for Ubuntu 20 are available in the GTIRB apt repository and may
be installed per the following instructions.

First, add GrammaTech's APT key.
```sh
wget -O - https://download.grammatech.com/gtirb/files/apt-repo/conf/apt.gpg.key | apt-key add -
```

Next update your sources.list file.
```sh
echo "deb [arch=amd64] https://download.grammatech.com/gtirb/files/apt-repo [distribution] [component]"| sudo tee -a /etc/apt/sources.list
```
Where:
- `[distribution]` is `focal` (currently, only Ubuntu 20 packages are available)
- `[component]` is either `stable`, which holds the last versioned release, or
`unstable`, which holds the HEAD of the repository.

Finally update your package database and install the core GTIRB tools:
```sh
sudo apt-get update
sudo apt-get install gtirb-pprinter ddisasm
```

**Warning**:  Stable versions gtirb-2.0.0, gtirb-pprinter-2.1.0, ddisasm-1.8.0
and OLDER rely on metapackages which cause conflicts if you try `apt-get upgrade`
(see https://github.com/GrammaTech/gtirb/issues/63).  In this case,
uninstall and reinstall the packages you got from the GTIRB repository.  You
may need to use `dpkg --remove` to remove the metapackages (e.g. `ddisasm`)
before removing the concrete versioned packages (e.g. `ddisasm-1.5.1`).
NEWER stable versions do not longer rely on metapackages and can be upgraded
without problems.

# Building

GTIRB's C++ API should successfully build in 64-bits with GCC, Clang,
and Visual Studio compilers supporting at least C++17.  GTIRB uses
CMake which must be installed with at least version 3.10.

The common build process looks like this:
```sh
mkdir build
cd build
# Note: You may wish to add some -D arguments to the next command. See below.
cmake <path/to/gtirb>
cmake --build .
# Run the test suite.
ctest
```

For customizing the GTIRB build, you can get a list of customization options by
navigating to your build directory and running:

```sh
cmake -LH
```

## Requirements

To build and install GTIRB, the following requirements should be installed:

- [CMake][], version 3.10.0 or higher.
   - Ubuntu 18 provides this version via the APT package `cmake`.
   - Ubuntu 16 and earlier provide out of date versions; build from
     source on those versions.
- [Protobuf][], version
  3.0.0 or later.
  - Ubuntu 18 provides this version via the APT packages
    `libprotobuf-dev` and `protobuf-compiler`.
  - Ubuntu 16 and earlier provide out of date versions; build from
    source on those versions.
- Boost [(non-standard Ubuntu package from launchpad.net)][], version 1.67 or later.
  - Ubuntu 18 only has version 1.65 in the standard repository.  See Ubuntu instructions above.

[CMake]: https://cmake.org/
[Protobuf]: https://developers.google.com/protocol-buffers/
[(non-standard Ubuntu package from launchpad.net)]: https://launchpad.net/~mhier/+archive/ubuntu/libboost-latest


# Usage

GTIRB is designed to be serialized using [Google protocol buffers][]
(i.e., [protobuf][]), enabling [easy and efficient use from any
programming language](#using-serialized-gtirb-data).

GTIRB may also be used through a dedicated API implemented in multiple
languages. The APIs provide efficient data structures suitable for use
by binary analysis and rewriting applications; see
[below](#gtirb-api-implementations) for details.

[Google protocol buffers]: https://developers.google.com/protocol-buffers/
[protobuf]: https://github.com/google/protobuf/wiki


## Using Serialized GTIRB Data

GTIRB uses a serialized format that consists of an 8-byte signature
followed by serialized [protobuf][] data. The protobuf data allows
for exploration and manipulation in the language of your choice.
The [Google protocol buffers][] homepage lists the languages in which
protocol buffers can be used directly; users of other languages can
convert the protobuf-formatted data to JSON format and then use the
JSON data in their applications.

The `proto` directory in this repository contains the protocol buffer
message type definitions for GTIRB. You can inspect these `.proto`
files to determine the structure of the various GTIRB message
types. The top-level message type is `IR`.

For more details, see [Using Serialized GTIRB Data](PROTOBUF.md).


## GTIRB API Implementations

The GTIRB API is currently available in C++, Python, and Common Lisp.
There is a *partial* Java API which is not ready for external use.
For language-independent API information, see [GTIRB
Components](doc/general/ComponentsIndex.md). For information about the
different API implementations, see:

  - [C++ API](doc/cpp/README.md)
  - [Python API](python/README.md)
  - [Common Lisp API](cl/README.md)
  - Java API **incomplete**
