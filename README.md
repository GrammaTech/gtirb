GTIRB
=====

The GrammaTech Intermediate Representation for Binaries (GTIRB) is a
machine code analysis and rewriting data structure.  It is intended to
facilitate the communication of binary IR between programs performing
binary disassembly, analysis, transformation, and pretty printing.
GTIRB is modeled on LLVM-IR, and seeks to serve a similar
functionality of encouraging communication and interoperability
between tools.

The remainder of this file has information on GTIRB's:
- [Structure](#structure)
- [Building](#building)
- [Usage](#usage)

## Structure
GTIRB has the following structure:

      Data Tables
      /
    IR        -----Data---Bytes
      \      /-----Symbols
      Modules------SymbolicExpressions
             \-----ImageByteMap
              -----ICFG
                   /  \
              Blocks  Edges
                 |
            Instructions---Bytes

### IR
An instance of GTIRB may include multiple `module`s which represent
loadable objects such as executables or libraries.  Each `module`
holds information such as `symbol`s, `data`, and an inter-procedural
control flow graph (`ICFG`).  The `ICFG` consists of basic `block`s
and control flow edges between these `block`s.  Each `block` holds
some number of `instruction`s.  Each `datum` and `instruction` holds
both a pointer to a range of bytes in the `ImageByteMap` and
`symbolic` information coverage that range.

### Instructions

GTIRB explicitly does NOT represent instructions but does provide
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
to GTIRB in the form of data tables.  These use variants to store
maps and vectors of various common GTIRB types. This repository will
describe the anticipated structure for very common data tables.

### UUIDs
Every element of GTIRB (namely: modules (`Module`), symbols
(`Symbol`), globals, blocks (`Block`), and instructions
(`InstructionRef`) has a unique associated ID.

## Building

GTIRB should successfully build in 64-bits with GCC, Clang, and
Visual Studio compilers supporting at least C++14. GTIRB requires
Boost.

1. **Install CMake**

   Obtain CMake from [here](https://cmake.org/download/) or via your
   package manager (e.g. `yum`, `apt-get`). It is advised to download a
   stable release and not a release candidate. For Mac/Windows check
   the option that adds CMake to the system path for all users.

2. **Install Git**

   Git is required to fetch the source code. Install with a package
   manager or from https://git-scm.com/download/win on Windows. Choose
   the option that adds git and minimal tools to the path.

3. **Install Dependencies**

    - *Linux*

      Use your local package manager to install Boost libraries. Boost
      1.59 or later is required.

      ```bash
      > sudo apt-get install boost
      ```

    - *Windows*

      For Windows, `vcpkg` can handle the dependencies.

      First, you have to install `vcpkg` from its git repository. From
      a command line, in the working directory:

      ```bash
      C:\vcpkg> git clone https://github.com/Microsoft/vcpkg.git .
      ```

      Then, you have to follow the instructions from the `vcpkg`
      documentation. Normally, during the installation process, it
      will detect the installed CMake.

      ```bash
      C:\vcpkg> .\bootstrap-vcpkg.bat
      C:\vcpkg> .\vcpkg integrate install
      ```

      This may give you the name of a file for use with CMake.  If so,
      note it.  You can use this in the CMake configuration later to
      help it find Boost.

      Then, you can install the dependencies:

      ```bash
      C:\vcpkg> .\vcpkg install boost
      ```

4. **Build with CMake**

   We recommend using the CMake GUI. (`cmake-gui`)

   Do not do an in-source build.  Specify the location for where to
   build the binaries (typically a folder called `build`).  It will
   make the directory if it does not already exist.

   If you insist on using the command line, then you can do this:

   ```bash
   /path/to/gtirb> cmake ./ -Bbuild
   ```

   By default, GTIRB is built with C++14 enabled, If you get an error
   stating "`CXX_STANDARD is set to invalid value '14'`", then you have
   an old compiler and will need to update.
  
   Once CMake configures and generates the project, you will have a
   native Makefile or Visual Studio Solution inside of
   `/path/to/gtirb/build`.

   On Linux, simply run `make`.

   ```bash
   /path/to/gtirb/build> make -j
   ```

5. **Run the Tests**

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

GTIRB is designed to be serialized using [Google's protocol
Buffers](https://developers.google.com/protocol-buffers/) (i.e.,
[protobuf](https://github.com/google/protobuf/wiki)) and eventually
JSON through protobuf's JSON output, enabling easy use from any
programming language.  GTIRB may also be used as a C++ library
implementing an efficient data structure suitable for use by binary
analysis and rewriting applications.

This repository defines the GTIRB data structure and C++ library.

### Populating the IR

Every IR holds a set of modules.

```c++
ir.getModules().emplace_back();
Module& module = ir.getModules().back();
```

Effective addresses are represented by a distinct type which can be
implicitly converted to uint64_t, but must be constructed explicitly.

```c++
Addr textSectionAddress(1328);
```

Create some sections:
```c++
module.getSections().emplace_back(".text", textSectionAddress, 466);
module.getSections().emplace_back(".data", textSectionAddress + Addr(466), 2048);
```

Create some data objects. These only define the layout and do not
directly store any data.

```c++
DataSet& data = module.getData();
data.emplace_back(Addr(2608), 6);
data.emplace_back(Addr(2614), 2);
```

The actual data is stored in the module's ImageByteMap:

```c++
ImageByteMap& byteMap = module.getImageByteMap();
byteMap.setAddrMinMax({Addr(2608), Addr(2616)});
uint8_t bytes[] = {1, 0, 2, 0, 115, 116, 114, 108};
byteMap.setData(Addr(2608), bytes);
```

Symbols associate a name with an effective address (Addr). They can
also link to a Data object or Instruction.

```c++
SymbolSet& symbols = module.getSymbols();
addSymbol(symbols, Symbol(Addr(2608), // address
                          "data1",    // name
                          data[0],    // referent
                          Symbol::StorageKind::Extern));
addSymbol(symbols, Symbol(Addr(2614), "data2", data[1], Symbol::StorageKind::Extern));
```

GTIRB can store multiple symbols with the same effective address.

```c++
addSymbol(symbols, Symbol(Addr(2614), "duplicate", data[1], Symbol::StorageKind::Local));
```

Create some basic blocks. Like DataObjects, Blocks don't directly hold
any data. GTIRB does not directly represent instructions.

```c++
Block b1(Addr(466), Addr(472));
Block b2(Addr(472), Addr(480));
```

Blocks are stored in an interprocedural CFG.  The CFG preserves the
order of blocks.  It can be populated with edges to denote control
flow or populated without any edges simply to hold all the
instructions in the binary.

```c++
auto vertex1 = addBlock(module.getCFG(), std::move(b1));
auto vertex2 = addBlock(module.getCFG(), std::move(b2));
```

The `addBlock` function returns a vertex descriptor which can be used
to retrieve the block or add edges:

```c++
auto edge1 add_edge(vertex1, vertex2, mainModule.getCFG()).first;
```

Edges can have boolean or numeric labels:

```c++
module.getCFG()[edge1] = true;
```

Information on symbolic operands and data is stored in a map by EA:

```c++
const Symbol* data2 = findSymbols(module.getSymbols(), Addr(2614))[0];
module.getSymbolicExpressions().insert({Addr(472), SymAddrConst{0, NodeRef<Symbol>(*data2)}});
```

Finally, data tables can be used to store any additional data at the
IR level.  These use variants to support a variety of maps and vectors
of common GTIRB types.

```c++
ir.addTable("eaTable", std::vector<Addr>({Addr(1), Addr(2), Addr(3)}));
ir.addTable("stringMap",
            std::map<std::string, table::ValueType>({{"a", "str1"}, {"b", "str2"}}));
```

### Querying the IR

Many components of GTIRB are stored in standard containers for ease
of use. However, some require special consideration.

Symbols can be looked up by effective address (Addr).  Any number of
symbols can share an effective address, so be prepared to deal with
multiple results.

```c++
std::vector<Symbol*> syms = findSymbols(module.getSymbols(), Addr(2614));
assert(syms.size() == 2);
assert(syms[0]->getName() == "data2");
assert(syms[1]->getName() == "duplicate");
```

Use a symbol's referent (either an InstructionRef or DataObject) to get
more information about the object to which the symbol
points. `NodeRef` behaves like a pointer and may be null.

```c++
NodeRef<DataObject> referent = syms[0]->getDataReferent();
assert(referent);
assert(referent->getAddress() == Addr(2614));
assert(referent->getSize() == 2);
```

The ICFG uses
[boost::graph](https://www.boost.org/doc/libs/1_67_0/libs/graph/doc/).
GTIRB also provides a convenience function for iterating over blocks:

```c++
for (const auto& b : blocks(module.getCFG())) {
  std::cout << "Block: " << b.getAddress() << ".." << addressLimit(b) << "\n";
}
```

Data tables are variants, which have to be resolved to the correct
type before use. This will throw an exception if the wrong type is
requested.

```c++
auto eaTable = boost::get<std::vector<Addr>>(ir.getTable("eaTable"));
for (auto ea : *eaTable) {
  std::cout << "EA: " << ea.get() << "\n";
}

auto stringMap = boost::get<std::map<std::string, table::ValueType>>(ir.getTable("stringMap"));
for (auto p : *stringMap) {
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
