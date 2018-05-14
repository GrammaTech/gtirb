GT-IRB
======

The GrammaTech Intermediate Representation for Binaries (GT-IRB) is a
machine code analysis and rewriting data structure.  It is intended to
facilitate the communication of binary IR between programs performing
binary disassembly, analysis, transformation, and pretty printing.
GT-IRB is modeled on LLVM-IR, and seeks to serve a similar
functionality of encouraging communication and interoperability
between tools.

The remainder of this file has information on GT-IRB's:
- [Structure](#structure)
- [Usage](#usage)
- [Building](#building)

## Structure
GT-IRB has the following structure:

          IR    -----Symbols
           |   /-----Relocations            Data Tables
        Modules------ErrorHandling          ----+------
           |   \-----Data-----Symbolic      ID1 | DATA1
         ICFG          \------Bytes         ID2 | DATA2
           |                                ID3 | DATA3
    Blocks & Edges                          ...
           |
      Instructions-------Symbolic
                 \-------Bytes

### IR
An instance of GT-IRB may include multiple `module`s which represent
loadable objects such as executables or libraries.  Each `module`
holds a list of `symbol`s, a list of `global`s, optional
`error-handling` information, and an inter-procedural control flow
graph (`ICFG`).  The `ICFG` consists of basic `block`s and control
flow edges between these `blocks`.  Each `block` holds some number of
`instructions`.

### Instructions

GT-IRB explicitly does NOT represent instructions but does provide
symbolic operand information and access to the bytes.  There are many
options for representation of single instructions (e.g.,
[BAP](https://github.com/BinaryAnalysisPlatform/bap)'s
[BIL](https://github.com/BinaryAnalysisPlatform/bil/releases/download/v0.1/bil.pdf),
or [Angr](http://angr.io)'s [Vex](https://github.com/angr/pyvex)).
Instruction bytes may easily be decoded/encoded using the popular
[Capstone](https://www.capstone-engine.org)/[Keystone](https://www.keystone-engine.org)
disassembler/assembler.

### Data Tables

Additional arbitrary information, e.g. analysis results, may be added
to GT-IRB in the form of data tables.  These tables are keyed by IDs.
Every element of GT-IRB (namely: `module`s, `symbol`s, `global`s,
`block`s, and `instruction`s) has a unique associated ID.  Two common
data table will be a table of function information and a table of
section information.  This repository will describe the anticipated
structure for very common data tables.

## Usage

GT-IRB is designed to be serialized to/from JSON, enabling easy use
from any programming language.  GT-IRB is also used as a C++ library
implementing an efficient data structure suitable for use by binary
analysis and rewriting applications.

This repository defines the GT-IRB data structure and C++ library, and
builds the following tools for the *manipulation*, *comparison*,
*sub-setting*, and *validation* of GT-IRB:

| Tool               | Description                                |
|--------------------|--------------------------------------------|
| `gtirb validate` | Validate an instance of gt-irb             |
| `gtirb compare`  | Compare two instances of gt-irb            |
| `gtirb select`   | Select a subset of gt-irb                  |
| `gtirb combine`  | Combine some number of instances of gt-irb |

## Building

GT-IRB should successfully build in 64-bits with GCC, Clang, and
Visual Studio compilers supporting at least C++14 (preferably C++17)
and uses the Boost libraries.

### Installing CMake (For CMake builds only)

The first thing to do is get hold of CMake. You can get it from [here](https://cmake.org/download/)
or via your package manager (e.g. `yum`, `apt-get`). It is advised to
download a stable release and not a release candidate. For Mac/Windows
check the option that adds CMake to the system path for all users.

### Installing Git

Git is required to fetch the source code. Install with a package
manager or from https://git-scm.com/download/win on Windows. Choose
the option that adds git and minimal tools to the path.

### Installing Dependencies

#### Linux

Use your local package manager to install Boost libraries.

```bash
> sudo apt-get install boost
```

#### Windows

For Windows, `vcpkg` can handle the dependencies.

First, you have to install `vcpkg` from its git repository. From a command line, in the working directory:

```
C:\vcpkg> git clone https://github.com/Microsoft/vcpkg.git .
```

Then, you have to follow the instructions from the `vcpkg`
documentation. Normally, during the installation process, it will
detect the installed CMake.

```
C:\vcpkg> .\bootstrap-vcpkg.bat
C:\vcpkg> .\vcpkg integrate install
```

This may give you the name of a file for use with CMake.  If so, note
it.  You can use this in the CMake configuration later to help it find
Boost.

Then, you can install the dependencies:

```
C:\vcpkg> .\vcpkg install boost
```

### Building with CMake

I recommend using the CMake GUI. (`cmake-gui`)

Do not do an in-source build.  Specify the location for where to build
the binaries (typically a folder called `build`).  It will make the
directory if it does not already exist.

If you insist on using the command line, then you can do this:

```bash
/path/to/gtirb> cmake ./ -Bbuild
```

By default, GT-IRB is built with C++17 enabled, If you get an error
stating "`CXX_STANDARD is set to invalid value '17'`", then you have
an old compiler.  You can either update your compiler or you can use
C++14 instead.  Use the variable `GTIRB_ISO_CPP_VERSION` and set it to
`C++14`.  This is a simple drop-down in the GUI, or you can set it
from the command line.

```bash
/path/to/gtirb> cmake ./ -Bbuild -DGTIRB_ISO_CPP_VERSION=C++14
```

Once CMake configures and generates the project, you will have a
native Makefile or Visual Studio Solution inside of
`/path/to/gtirb/build`.

On Linux, simply run `make`.

```bash
/path/to/gtirb/build> make -j
```

### Running the Tests

Go into the `bin` folder and execute `TestGTIRB`.

```bash
/path/to/gtirb/bin> ./TestGTIRB
```

Alternately, a CMake target called `test` was created that mimics Autotools.  This can be used to automatically run the tests after a build.

```bash
/path/to/gtirb/build> make test
```
