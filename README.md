GTIRB
=====

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


## Structure

GTIRB has the following structure.  Solid lines denote inheritance.
Dotted lines denote reference by UUID.

       +--IPCFG--Edges...........................
       |                 .                      .
       |         +-ProxyBlocks             +-Blocks
       |         |                         |
    IR-+-Modules-+-Sections--ByteIntervals-+
       |    |    |                         |
       |    |    +-Symbols.........        +-SymbolicExpressions
       +-AuxData                  .              .
        --------                  ................
        ID0|DATA0
        ID1|DATA1....*anything*
        ID2|DATA2


### IR

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


### Instructions

GTIRB explicitly does NOT represent instructions or instruction
semantics but does provide symbolic operand information and access to
the bytes.  There are many *intermediate language*s (IL)s for
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


### Auxiliary Data

GTIRB provides for the sharing of additional information,
e.g. analysis results, in the form of `AuxData` objects.  These can
store maps and vectors of basic GTIRB types in a portable way. The
[GTIRB manual](https://grammatech.github.io/gtirb/) describes the
structure for common types of auxiliary data such as function boundary
information, type information, or results of common analyses in
[Standard AuxData Schemata](https://grammatech.github.io/gtirb/md__aux_data.html).


### UUIDs

Every element of GTIRB---e.g., modules (`Module`), symbols (`Symbol`),
and blocks (`Block`)---has a universally unique identifier (UUID).
UUIDs allow both first-class IR components and AuxData tables to
reference elements of the IR.

Instructions and symbolic operands can be addressed by the class
`Offset` which encapsulates a UUID (that refers to the instruction's
block) and an offset.


## Building

GTIRB should successfully build in 64-bits with GCC, Clang, and Visual
Studio compilers supporting at least C++17.  GTIRB uses CMake which
must be installed with at least version 3.9.

The common build process looks like this:
```sh
mkdir build
cd build
# Note: You may wish to add some -D arguments to the next command. See below.
cmake ../path/to/gtirb
cmake --build .
# Run the test suite.
./bin/TestGTIRB
```

The gtirb library will be located under `build/lib`.

### Requirements

To build and install GTIRB, the following requirements should be installed:

- [Protobuf](https://developers.google.com/protocol-buffers/), version 3.0.0 or later.
  - Ubuntu 18 provides this version via `libprotobuf-dev` and `protobuf-compiler`.
    Ubuntu 16 and earlier provide out of date versions;
    build from source on those versions.
- [Boost](https://www.boost.org/), version 1.67.0 or later.
  - No version of Ubuntu provides this version of Boost yet;
    you must build it from source.

### Building on Windows

CMake can optionally use a toolchain file, as generated by
[Microsoft's vcpkg](https://github.com/Microsoft/vcpkg), to find packages like
boost or protobuf on Windows. One way to install GTIRB's dependencies is to run
`vcpkg` before running `cmake`:

```
vcpkg.exe install --triplet x64-windows protobuf boost
```

and pass the path to the toolchain file when executing the CMake command above:

```
    -DCMAKE_TOOLCHAIN_FILE="C:\path\to\vcpkg\scripts\buildsystems\vcpkg.cmake"
```

## Usage

GTIRB is designed to be serialized using [Google's protocol
buffers](https://developers.google.com/protocol-buffers/) (i.e.,
[protobuf](https://github.com/google/protobuf/wiki)), enabling [easy
and efficient use from any programming language](#using-serialized-gtirb-data).

GTIRB may also be [used as a C++ library](#using-the-c++-api)
implementing an efficient data structure suitable for use by binary
analysis and rewriting applications.

- [Using Serialized GTIRB Data](#using-serialized-gtirb-data)
- [Using the C++ Library](#using-the-c++-library)

### Using Serialized GTIRB Data

The serialized [protobuf](https://github.com/google/protobuf/wiki)
data produced by GTIRB allows for exploration and manipulation in the
language of your choice. The [Google protocol
buffers](https://developers.google.com/protocol-buffers/) homepage
lists the languages in which protocol buffers can be used directly;
users of other languages can convert the protobuf-formatted data to
JSON format and then use the JSON data in their applications. In the
future we intend to define a standard JSON schema for GTIRB.

Directory `gtirb/src/proto` contains the protocol buffer message type
definitions for GTIRB. You can inspect these `.proto` files to
determine the structure of the various GTIRB message types. The
top-level message type is `IR`.

For more details, see [Using Serialized GTIRB Data](PROTOBUF.md)


### Using the C++ Library

We have provided several C++ examples in directory
`gtirb/doc/examples`. See the [Examples tab](examples.html) for more
information.

The remainder of this section provides examples walking through common
tasks using the GTIRB C++ library API.

- [Populating the IR](#populating-the-ir)
- [Querying the IR](#querying-the-ir)
- [Serialization](#serialization)

Also note that to compile a C++ client that uses the GTIRB library,
you have to inform the compiler and linker where to find GTIRB's
header files and library archives. In addition, the OS also needs to
be informed about where to find GTIRB's dynamic libraries. These files
are located under `include` and `lib` in the build output directory
you picked when running CMake originally. How to do this will depend
on the particular compiler tool chain and context you are working
with.


#### Populating the IR

GTIRB objects are created within a `Context` object. Freeing the
`Context` will also destroy all the objects within it.

```c++
Context C;
IR& ir = *IR::Create(C);
```

Every IR holds a set of modules.

```c++
ir.addModule(Module::Create(C));
Module& module = ir.modules()[0];
```

Addresses are represented by a distinct type which can be
explicitly converted to and from `uint64_t`.

```c++
Addr textSectionAddress(1328);
```

Create some sections:
```c++
module.addSection(Section::Create(C, ".text", textSectionAddress, 466));
module.addSection(
    Section::Create(C, ".data", textSectionAddress + 466, 2048));
```

Create some data objects. These only define the layout and do not
directly store any data.

```c++
auto* data1 = DataObject::Create(C, Addr(2608), 6);
auto* data2 = DataObject::Create(C, Addr(2614), 2);
module.addData(data1);
module.addData(data2);
```

The actual data is stored in the module's ImageByteMap:

```c++
ImageByteMap& byteMap = module.getImageByteMap();
byteMap.setAddrMinMax({Addr(2608), Addr(2616)});
std::array<uint8_t, 8> bytes{1, 0, 2, 0, 115, 116, 114, 108};
byteMap.setData(Addr(2608), bytes);
```

Symbols associate a name with an object in the `IR`, such as a
`DataObject` or `Block`. They can optionally store an address instead.

```c++
auto data = module.data();
module.addSymbol(Symbol::Create(C,
                                data1,      // referent
                                "data1",    // name
                                Symbol::StorageKind::Extern));
module.addSymbol(Symbol::Create(C, data2, "data2",
                                Symbol::StorageKind::Extern));
```

GTIRB can store multiple symbols with the same address or referent.

```c++
module.addSymbol(Symbol::Create(C, data2, "duplicate",
                                Symbol::StorageKind::Local));
module.addSymbol(Symbol::Create(C, Addr(2608), "alias"))
```


Basic blocks are stored in an interprocedural CFG. Like `DataObjects`,
`Blocks` reference data in the `ImageByteMap` but do not directly hold
any data themselves. GTIRB does not directly represent instructions.

```c++
auto& cfg = module.getCFG();
auto* b1 = emplaceBlock(cfg, C, Addr(466), 6);
auto* b2 = emplaceBlock(cfg, C, Addr(472), 8);
```

The `CFG` can be populated with edges to denote control flow. Or edges
can be omitted and the `CFG` used simply as a container for `Blocks`..

```c++
auto edge1 add_edge(vertex1, vertex2, mainModule.getCFG()).first;
```

Edges can have boolean or numeric labels:

```c++
module.getCFG()[edge1] = true;
module.getCFG()[edge2] = 1;
```

Information on symbolic operands and data is indexed by address:

```c++
Symbol* dataSym = &*module.findSymbols(Addr(2614)).begin();
module.addSymbolicExpression(Addr(472), SymAddrConst{0, dataSym});
```

Finally, auxiliary data can be used to store additional data at the IR
level. An `AuxData` object can store integers, strings, basic GTIRB
types such as `Addr` and `UUID`, and tuples or containers over these
types.

```c++
ir.addAuxData("addrTable", std::vector<Addr>({Addr(1), Addr(2), Addr(3)}));
ir.addAuxData("stringMap", std::map<std::string, std::string>(
                             {{"a", "str1"}, {"b", "str2"}}));
```


#### Querying the IR

Symbols can be looked up by address or name.  Any number of symbols
can share an address or name, so be prepared to deal with multiple
results.

```c++
auto syms = module.findSymbols(Addr(2614));
auto it = syms.begin();
Symbol& sym1 = *it++;
assert(sym1.getName() == "data2");
assert((*it++).getName() == "duplicate");

auto& sym2 = *module.findSymbols("data1").begin();
assert(sym2.getAddress() == Addr(2608));
```

Use a symbol's referent (either a Block or DataObject) to get
more information about the object to which the symbol
points.

```c++
DataObject* referent = sym1.getReferent<DataObject>();
assert(referent);
assert(referent->getAddress() == Addr(2614));
assert(referent->getSize() == 2);
```

Alternatively, DataObjects can be looked up by an address contained
within the object. Any number of objects may overlap and contain an
address, so be prepared to deal with multiple results.

```c++
auto objs = module.findData(Addr(2610));
assert(objs.size() == 1);
assert(objs.begin()->getAddress() == Addr(2608));
```

The CFG uses
[boost::graph](https://www.boost.org/doc/libs/1_67_0/libs/graph/doc/).
GTIRB also provides a convenience function for iterating over blocks:

```c++
for (const auto& b : blocks(cfg)) {
  std::cout << "Block: " << uint64_t(b.getAddress()) << ".."
            << uint64_t(addressLimit(b)) << "\n";
}
```

`Blocks` contain a `vertex_descriptor` which is used to look up
corresponding information in the `CFG`:

```c++
auto [edgeDescriptor, exists] = edge(b1->getVertex(), b2->getVertex(), cfg);
assert(exists);
```

`edge_descriptors` can be used to look up labels and the source/target
blocks:

```c++
auto edgeRange = edges(cfg);
for (auto it = edgeRange.first; it != edgeRange.second; it++) {
  auto e = *it;
  auto v1 = source(e, cfg);
  auto v2 = target(e, cfg);
  std::cout << "Edge: " << uint64_t(cfg[v1]->getAddress()) << " => "
            << uint64_t(cfg[v2]->getAddress());
  if (auto* b = std::get_if<bool>(&cfg[e])) {
    std::cout << ": " << *b;
  }
  std::cout << "\n";
}
```

Data have to be resolved to the correct type with the `get()` method
before use. This will return null if the wrong type is requested.

```c++
auto addrTable = ir.getAuxData("addrTable")->get<std::vector<Addr>>();
for (auto addr : *addrTable) {
  std::cout << "Addr: " << uint64_t(addr) << "\n";
}

auto* stringMap =
    ir.getAuxData("stringMap")->get<std::map<std::string, std::string>>();
for (auto p : *stringMap) {
  std::cout << p.first << " => " << p.second << "\n";
}
```

#### Serialization

Serialize IR to a file:

```c++
std::ofstream out("path/to/file");
ir.save(out);
```

Deserialize from a file:

```c++
std::ifstream in("path/to/file");
IR& newIR = *IR::load(C, in);
```
