# Using the C++ Library

We have provided several C++ examples in directory
`gtirb/doc/examples`. See the [Examples page](../examples.html) for more
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


## Populating the IR

GTIRB representation objects have class gtirb::IR, and are created within a
gtirb::Context object. Freeing the `Context` will also destroy all the
objects within it.

```cpp
Context C;                
IR& ir = *IR::Create(C);
```

- gtirb::IR::Create()


Every IR holds a set of modules (gtirb::Module).

```cpp
ir.addModule(Module::Create(C));    
Module& module = ir.modules()[0];
```

- gtirb::IR::addModule()
- gtirb::IR::modules()
- gtirb::Module::Create()


Addresses are represented by a distinct type, gtirb::Addr, which can be
explicitly converted to and from `uint64_t`.

```cpp
Addr textSectionAddress(1328);
```

- gtirb::Addr::Addr(gtirb::Addr::value_type)


Create some sections (gtirb::Section) and add them to the module.

```cpp
module.addSection(Section::Create(C, ".text", textSectionAddress, 466));
module.addSection(
   Section::Create(C, ".data", textSectionAddress + 466, 2048));
```

- gtirb::Section::Create()
- gtirb::Module::addSection()


Create some data objects. These only define the layout and do not
directly store any data.

```cpp
auto* data1 = DataObject::Create(C, Addr(2608), 6);
auto* data2 = DataObject::Create(C, Addr(2614), 2);
module.addData(data1);
module.addData(data2);
```

The actual data is stored in the module's ImageByteMap:

```cpp
ImageByteMap& byteMap = module.getImageByteMap();
byteMap.setAddrMinMax({Addr(2608), Addr(2616)});
std::array<uint8_t, 8> bytes{1, 0, 2, 0, 115, 116, 114, 108};
byteMap.setData(Addr(2608), bytes);
```

Symbols (gtirb::Symbol) associate a name with an object in the `IR`, such as a
`DataObject` or `Block`. They can optionally store an address instead.

```cpp
auto data = module.data();
module.addSymbol(Symbol::Create(C,
                                data1,      // referent
                                "data1",    // name
                                Symbol::StorageKind::Extern));
module.addSymbol(Symbol::Create(C, data2, "data2",
                                Symbol::StorageKind::Extern));
```

- gtirb::Module::addSymbol()
- gtirb::Symbol::StorageKind


GTIRB can store multiple symbols with the same address or referent.

```cpp
   module.addSymbol(Symbol::Create(C, data2, "duplicate",
                                   Symbol::StorageKind::Local));
   module.addSymbol(Symbol::Create(C, Addr(2608), "alias"))
```

Basic blocks are stored in an interprocedural control flow graph
(gtirb::CFG). Like `DataObjects`, `Blocks` reference data in the
`ImageByteMap` but do not directly hold any data themselves. GTIRB
does not directly represent instructions.

```cpp
auto& cfg = module.getCFG();
auto* b1 = emplaceBlock(cfg, C, Addr(466), 6);
auto* b2 = emplaceBlock(cfg, C, Addr(472), 8);
```

- gtirb::Module::getCFG()


The `CFG` can be populated with edges to denote control flow. Or edges
can be omitted and the `CFG` used simply as a container for `Blocks`..

```cpp
auto edge1 add_edge(vertex1, vertex2, mainModule.getCFG()).first;
```

Edges can have boolean or numeric labels:

```cpp
module.getCFG()[edge1] = true;
module.getCFG()[edge2] = 1;
```

Information on symbolic operands and data is indexed by address:

```cpp
Symbol* dataSym = &*module.findSymbols(Addr(2614)).begin();
module.addSymbolicExpression(Addr(472), SymAddrConst{0, dataSym});
```

- gtirb::Module::findSymbols()
- gtirb::SymAddrConst


Finally, auxiliary data can be used to store additional data at the IR
level. A gtirb::AuxData object can store integers, strings, basic GTIRB
types such as gtirb::Addr and gtirb::UUID, and tuples or containers over these
types.

```cpp
ir.addAuxData("addrTable", std::vector<Addr>({Addr(1), Addr(2), Addr(3)}));
ir.addAuxData("stringMap",
              std::map<std::string, std::string>({{"a", "str1"}, {"b", "str2"}}));
```

- gtirb::IR::addAuxData()


## Querying the IR

Symbols can be looked up by address or name.  Any number of symbols
can share an address or name, so be prepared to deal with multiple
results.

```cpp
auto syms = module.findSymbols(Addr(2614));
auto it = syms.begin();
Symbol& sym1 = *it++;
assert(sym1.getName() == "data2");
assert((*it++).getName() == "duplicate");

auto& sym2 = *module.findSymbols("data1").begin();
assert(sym2.getAddress() == Addr(2608));
```

- gtirb::Module::findSymbols()
- gtirb::Symbol::getName()


Use a symbol's referent (either a Block or DataObject) to get
more information about the object to which the symbol
points.

```cpp
DataObject* referent = sym1.getReferent<DataObject>();
assert(referent);
assert(referent->getAddress() == Addr(2614));
assert(referent->getSize() == 2);
```

- gtirb::Symbol::getReferent()


Alternatively, DataObjects can be looked up by an address contained
within the object. Any number of objects may overlap and contain an
address, so be prepared to deal with multiple results.

```cpp
auto objs = module.findData(Addr(2610));
assert(objs.size() == 1);
assert(objs.begin()->getAddress() == Addr(2608));
```

The CFG uses
[boost::graph](https://www.boost.org/doc/libs/1_67_0/libs/graph/doc/).
GTIRB also provides a convenience function for iterating over blocks:

```cpp
for (const auto& b : blocks(cfg)) {
  std::cout << "Block: " << uint64_t(b.getAddress()) << ".."
            << uint64_t(addressLimit(b)) << "\n";
}
```

`Blocks` contain a `vertex_descriptor` which is used to look up
corresponding information in the `CFG`:

```cpp
auto [edgeDescriptor, exists] = edge(b1->getVertex(), b2->getVertex(), cfg);
assert(exists);

```


`edge_descriptors` can be used to look up labels and the source/target
blocks:

```cpp
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

```cpp
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

- gtirb::IR::getAuxData()
- gtirb::AuxData::get()


## Serialization

Serialize IR to a file with gtirb::IR::save().

```cpp
std::ofstream out("path/to/file");
ir.save(out);
```

Deserialize from a file with gtirb::IR::load().

```cpp
std::ifstream in("path/to/file");
IR& newIR = *IR::load(C, in);
```
