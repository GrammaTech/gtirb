GT-IRB
======

The GrammaTech Intermediate Representation for Binaries (GT-IRB) is a machine code analysis and rewriting infrastructure.  The GT-IRB is based on GrammaTech's CSurf/SWYX IR.

## Project Overview

We have promised to deliver a "binary code IR tailored for rewriting" called GT-IRB.  GT-IRB will be published and available for other performers to write front-ends, analyses, and back-ends against. GT-IRB will be based as much as possible on our existing CSurf/SWYX IR, specifically it will be the subset of our existing IR which we determine to be (i) general and (ii) minimally sufficient for binary rewriting.  Our efforts will include the following:

1.  Definition of a specification of GT-IRB.
2.  Refactoring of CSurf/SWYX to use the specified GT-IRB.
3.  Implementation of translation facilities to produce GT-IRB from the CSurf/SWYX front-end and consume GT-IRB in CSurf/SWYX back-end.
4.  Ongoing modularization of CSurf/SWYX into separately compilable components which communicate via GT-IRB.  An initial set of useful components may be: (i) a COTS-binary to GT-IRB front-end, (ii) a GT-IRB to GT-IRB back-end, and (iii) a GT-IRB to assembler pretty printer.

## Building

GT-IRB should successfully build in 64-bits with GCC, Clang, and Visual Studio compilers supporting at least C++14 (preferably C++17) and uses the Boost libraries.

### Installing CMake (For CMake builds only)

The first thing to do is get hold of CMake. You can get it from here or via your package manager (e.g. `yum`, `apt-get`). It is advised to download a stable release and not a release candidate. For Mac/Windows check the option that adds CMake to the system path for all users.

### Installing SCons (For SCons builds only)

[TBD]

### Installing Git

Git is required to fetch the source code. Install with a package manager or from https://git-scm.com/download/win on Windows. Choose the option that adds git and minimal tools to the path.

### Installing Dependencies

#### Linux

Use your local package manager to install Boost libraries.

```
> sudo apt-get install boost
```

#### Windows

For Windows, `vcpkg` can handle the dependencies.

First, you have to install `vcpkg` from its git repository. From a command line, in the working directory:

```
C:\vcpkg> git clone https://github.com/Microsoft/vcpkg.git .
```

Then, you have to follow the instructions from the `vcpkg` documentation. Normally, during the installation process, it will detect the installed CMake.

```
C:\vcpkg> .\bootstrap-vcpkg.bat
C:\vcpkg> .\vcpkg integrate install
```

This may give you the name of a file for use with CMake.  If so, note it.  You can use this in the CMake configuration later to help it find Boost.

Then, you can install the dependencies:

```
C:\vcpkg> .\vcpkg install boost
```

### Building with CMake

I recommend using the CMake GUI. (`cmake-gui`) 

Do not do an in-source build.  Specify the location for where to build the binaries (typically a folder called `build`).  It will make the directory if it does not already exist.

If you insist on using the command line, then you can do this:

`/path/to/gtirb> cmake ./ -Bbuild`

By default, GT-IRB is built with C++17 enable.d  If you get an error stating "`CXX_STANDARD is set to invalid value '17'`", then you have an old compiler.  You can either update your compiler or you can use C++14 instead.  Use the variable `GTIRB_ISO_CPP_VERSION` and set it to `C++14`.  This is a simple drop-down in the GUI, or you can set it from the command line.

`/path/to/gtirb> cmake ./ -Bbuild -DGTIRB_ISO_CPP_VERSION=C++14`

Once CMake configures and generates the project, you will have a native Makefile or Visual Studio Solution inside of `/path/to/gtirb/build`.  

On linux, simply run `make`.

`/path/to/gtirb/build> make -j`

### Building with SCons

[TBD]

### Running the Tests

Go into the `bin` folder and execute `TestGTIRB`.

`/path/to/gtirb/bin> ./TestGTIRB`
