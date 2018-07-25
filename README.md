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
- [Building](#building)
- [Usage](#usage)

## Structure
GT-IRB has the following structure:

              IR
            /    \        -----Symbols
           /      \      /-----Relocations
    Data Tables   Modules------ImageByteMap
                         \-----Data
                          -----Symbolic
                          -----ICFG
                               /  \
                          Blocks  Edges
                             |
                        Instructions


### IR
An instance of GT-IRB may include multiple `module`s which represent
loadable objects such as executables or libraries.  Each `module`
holds information such as `symbol`s, data objects, and an
inter-procedural control flow graph (`ICFG`).  The `ICFG` consists of
basic `block`s and control flow edges between these `blocks`.  Each
`block` holds some number of `instructions`.

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
to GT-IRB in the form of data tables.  These use variants to store
maps and vectors of various common GT-IRB types. This repository will
describe the anticipated structure for very common data tables.

### UUIDs
Every element of GT-IRB (namely: `module`s, `symbol`s, `global`s,
`block`s, and `instruction`s) has a unique associated ID.

## Building

GT-IRB should successfully build in 64-bits with GCC, Clang, and
Visual Studio compilers supporting at least C++14. GT-IRB requires
Boost.

### Installing CMake (For CMake builds only)

The first thing to do is get hold of CMake. You can get it from
[here](https://cmake.org/download/) or via your package manager
(e.g. `yum`, `apt-get`). It is advised to download a stable release
and not a release candidate. For Mac/Windows check the option that
adds CMake to the system path for all users.

### Installing Git

Git is required to fetch the source code. Install with a package
manager or from https://git-scm.com/download/win on Windows. Choose
the option that adds git and minimal tools to the path.

### Installing Dependencies

#### Linux

Use your local package manager to install Boost libraries. Boost 1.59
or later is required.

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

By default, GT-IRB is built with C++14 enabled, If you get an error
stating "`CXX_STANDARD is set to invalid value '14'`", then you have
an old compiler and will need to update.

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

Alternately, a CMake target called `check` was created that mimics
Autotools.  This can be used to automatically run the tests after a
build.

```bash
/path/to/gtirb/build> make check
```

## Usage

GT-IRB is designed to be serialized to/from JSON, enabling easy use
from any programming language.  GT-IRB is also used as a C++ library
implementing an efficient data structure suitable for use by binary
analysis and rewriting applications.

This repository defines the GT-IRB data structure and C++ library.

### Populating the IR

Every IR begins with a single main module. Other modules can be
created as needed.

```c++
Module& mainModule = ir.getMainModule();
```

Effective addresses are represented by a distinct type which can be
implicitly converted to uint64_t, but must be constructed explicitly.

```c++
EA textSectionAddress(1328);
```

Create some sections:
```c++
mainModule.getSections().emplace_back(".text", 466, textSectionAddress);
mainModule.getSections().emplace_back(".data", 2048, textSectionAddress + EA(466));
```

Create some data objects. These only define the layout and do not
directly store any data.

```c++
DataSet& data = mainModule.getData();
data.emplace_back(EA(2608), 6);
data.emplace_back(EA(2614), 2);
```

The actual data is stored in the module's ImageByteMap:
```c++
ImageByteMap& byteMap = mainModule.getImageByteMap();
byteMap.setEAMinMax({EA(2608), EA(2616)});
uint8_t bytes[] = {1, 0, 2, 0, 115, 116, 114, 108};
byteMap.setData(EA(2608), as_bytes(gsl::make_span(bytes)));
```

Symbols associate a name with an EA. They can also link to a Data
object or Instruction.
```c++
SymbolSet& symbols = mainModule.getSymbols();
addSymbol(symbols, Symbol(EA(2608), // address
                          "data1",  // name
                          data[0],  // referent
                          Symbol::StorageKind::Extern));
addSymbol(symbols, Symbol(EA(2614), "data2", data[1], Symbol::StorageKind::Extern));
```

GT-IRB can store multiple symbols with the same EA. 
```c++
addSymbol(symbols, Symbol(EA(2614), "duplicate", data[1], Symbol::StorageKind::Local));
```

Instructions are organized into basic blocks .As without other GT-IRB
objects, Blocks and Instructions don't directly hold any data.

```c++
Block b1(EA(466), EA(472),
         {Instruction(EA(466), 2), Instruction(EA(468), 2), Instruction(EA(470), 2)});
Block b2(EA(472), EA(480), {Instruction(EA(472), 4), Instruction(EA(476), 4)});
```

Blocks are stored an interprocedural CFG. The CFG preserves the order
of blocks, so it can be used without any edges simply to hold all the
instructions in the binary.

```c++
auto vertex1 = addBlock(mainModule.getCFG(), std::move(b1));
auto vertex2 = addBlock(mainModule.getCFG(), std::move(b2));
```

`addBlock` returns a vertex descriptor which can be used to retrieve
the block or add edges:

```c++
add_edge(vertex1, vertex2, mainModule.getCFG());
```

Information on symbolic operands and data is stored in a map by EA:

```c++
const Symbol* data2 = findSymbols(mainModule.getSymbols(), EA(2614))[0];
mainModule.getSymbolicOperands().insert(
    {EA(472), SymAddrConst{0, NodeReference<Symbol>(*data2)}});
```


Finally, tables can be used to store additional data at the IR
level. These use variants to support a variety of maps and vectors of
common GT-IRB types.

```c++
ir.addTable("eaTable", std::vector<EA>({EA(1), EA(2), EA(3)}));
ir.addTable("stringMap",
            std::map<std::string, table::ValueType>({{"a", "str1"}, {"b", "str2"}}));
```

### Querying the IR

Many components of GT-IRB are stored in standard containers for ease
of use. However, some require special consideration.


Symbols can be looked up by EA. Any number of symbols can share an EA,
so be prepared to deal with multiple results.

```c++
std::vector<Symbol*> syms = findSymbols(mainModule.getSymbols(), EA(2614));
assert(syms.size() == 2);
assert(syms[0]->getName() == "data2");
assert(syms[1]->getName() == "duplicate");
```

Use a symbol's referent (either an Instruction or Data object) to get
more information about the object to which the symbol
points. `NodeReference` behaves like a pointer and may be null.

```c++
NodeReference<Data> referent = syms[0]->getDataReferent();
assert(referent);
assert(referent->getEA() == EA(2614));
assert(referent->getSize() == 2);
```

The CFG uses
[boost::graph](https://www.boost.org/doc/libs/1_67_0/libs/graph/doc/). GT-IRB
also provides a convenience function for iterating over blocks:

```c++
for(const auto& b : blocks(mainModule.getCFG()))
{
    std::cout << "Block: " << b.getStartingAddress().get() << ".." << b.getEndingAddress().get()
              << ", " << b.getInstructions().size() << " instructions\n";
}
```

Data tables are variants, which have to be resolved to the correct
type before use. This will throw an exception if the wrong type is
requested.

```c++
auto eaTable = boost::get<std::vector<EA>>(ir.getTable("eaTable"));
for(auto ea : *eaTable)
{
    std::cout << "EA: " << ea.get() << "\n";
}

auto stringMap = boost::get<std::map<std::string, table::ValueType>>(ir.getTable("stringMap"));
for(auto p : *stringMap)
{
    std::cout << p.first << " => " << boost::get<std::string>(p.second) << "\n";
}
```

### Serialization

Serialize IR to a file:

```c++
std::ofstream out("path/to/file");
ir.save(out);
```

Deserialize from a file:

```c++
IR newIR;
std::ifstream in("path/to/file");
newIR.load(in);
```
