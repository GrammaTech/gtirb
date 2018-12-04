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

GTIRB has the following structure:

      Aux Data
      /
    IR        -----Data---Bytes
      \      /-----Symbols
      Modules------SymbolicExpressions
             \-----ImageByteMap
              -----ICFG
                   /  \
               Edges  Blocks


### IR

An instance of GTIRB may include multiple modules (`Module`) which
represent loadable objects such as executables or libraries.  Each
module holds information such as symbols (`Symbol`), data (`Data`),
and an inter-procedural control flow graph (`ICFG`).  The ICFG
consists of basic blocks (`Block`) and control flow edges between
these blocks.  Each datum and each block holds both a pointer to a
range of bytes in the `ImageByteMap` and symbolic (`Symbol`)
information covering this range.


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


### Auxiliary Data

Additional arbitrary information, e.g. analysis results, may be added
to GTIRB in the form of `AuxData` objects.  These can store maps and
vectors of basic GTIRB types in a portable way. This repository will
describe the anticipated structure for very common types of auxiliary
data such as function boundary information.


### UUIDs

Every element of GTIRB (namely: modules (`Module`), symbols
(`Symbol`), globals, blocks (`Block`), and instructions
(`InstructionRef`) has a universally unique identifier (UUID).


## Building

GTIRB should successfully build in 64-bits with GCC, Clang, and Visual
Studio compilers supporting at least C++17.  GTIRB uses CMake which
must be installed.

    sh
    mkdir build
    cd build
    cmake ../path/to/gtirb
    make -j
    # Run the test suite.
    ./bin/TestGTIRB

The gtirb library will be located in `lib/libgtirb.so` in the build
directory.


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
JSON format and then use the JSON data in their applications. In the future we intend to define a standard JSON schema for GTIRB.

Directory `gtirb/src/proto' contains the protocol buffer message type
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
`DataObject` or `Block`. They can optionally store an address as well.

```c++
auto data = module.data();
module.addSymbol(Symbol::Create(C,
                                Addr(2608), // address
                                "data1",    // name
                                data1,      // referent
                                Symbol::StorageKind::Extern));
module.addSymbol(Symbol::Create(C, Addr(2614), "data2", data2,
                                Symbol::StorageKind::Extern));
```

GTIRB can store multiple symbols with the same address.

```c++
module.addSymbol(Symbol::Create(C, Addr(2614), "duplicate", data2,
                                Symbol::StorageKind::Local));
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

Use a symbol's referent (either an InstructionRef or DataObject) to get
more information about the object to which the symbol
points.

```c++
DataObject* referent = sym1.getReferent<DataObject>();
assert(referent);
assert(referent->getAddress() == Addr(2614));
assert(referent->getSize() == 2);
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
