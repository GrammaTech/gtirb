#include <gtirb/gtirb.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <fstream>
#include <iostream>

using namespace gtirb;

int main() {
  // BEGIN
  //
  // ### Initialization for AuxData usage
  //
  // To make use of the auxiliary data mechanism, each schema that is
  // intended to be used must be registered with the API before doing
  // anything else with GTIRB. Typically, this is easiest to do as one
  // of the first steps in main().
  AuxDataContainer::registerAuxDataType<gtirb::schema::Types>();
  //
  // ### Populating the IR
  //
  // GTIRB representation objects have class `gtirb::IR`, and are created within
  // a context object (`gtirb::Context`). Freeing the context will also destroy
  // all the objects within it.
  Context C;
  auto* Ir = IR::Create(C);

  // Every IR holds a set of modules (`gtirb::Module`).
  auto* M = Module::Create(C, "example");
  Ir->addModule(M);
  // Could also have written: auto* M = Ir->addModule(C);

  // Every module holds a set of sections (`gtirb::Section`).
  auto* S = Section::Create(C, ".text");
  M->addSection(S);
  // Could also have written: auto* S = M->addSection(C, ".text");

  // Every section has a set of byte intervals (`gtirb::ByteInterval`).
  auto* BI = ByteInterval::Create(C, Addr(2048), 466);
  S->addByteInterval(BI);
  // Could also have written: auto* BI = S->addByteInterval(C, Addr(2048), 466);

  // Create some data objects. These only define the layout and do not directly
  // store any data.
  auto* D1 = DataBlock::Create(C, 6);
  BI->addBlock(0, D1);
  auto* D2 = DataBlock::Create(C, 2);
  BI->addBlock(6, D2);
  // The actual data is stored in the blocks' byte interval:
  std::array<uint8_t, 8> Bytes{1, 0, 2, 0, 115, 116, 114, 108};
  BI->insertBytes(const_cast<const ByteInterval*>(BI)->bytes_begin<uint8_t>(),
                  Bytes.begin(), Bytes.end());
  // Symbols (`gtirb::Symbol`) associate a name with a block in the IR, such as
  // code blocks, data blocks, or proxy blocks. They can optionally store an
  // address instead.
  [[maybe_unused]] auto* Sym1 = M->addSymbol(Symbol::Create(C, D1, "data1"));
  [[maybe_unused]] auto* Sym2 = M->addSymbol(Symbol::Create(C, D2, "data2"));
  // GTIRB can store multiple symbols with the same address or referent.
  M->addSymbol(Symbol::Create(C, D2, "duplicateReferent"));
  M->addSymbol(Symbol::Create(C, Addr(2048), "duplicateName"));
  M->addSymbol(Symbol::Create(C, Addr(4096), "duplicateName"));
  // Basic blocks are stored as `gtirb::CodeBlock`s. Like data blocks, code
  // blocks reference data in a byte interval but do not directly hold any data
  // themselves. GTIRB does not directly represent instructions.
  auto* B1 = CodeBlock::Create(C, 4);
  BI->addBlock(12, B1);
  auto* B2 = CodeBlock::Create(C, 6);
  BI->addBlock(16, B2);
  // GTIRB has an interprocedural control flow graph (`gtirb::CFG`) to track
  // relations between code blocks. The `CFG` can be populated with edges to
  // denote control flow.
  auto& Cfg = Ir->getCFG();
  auto E = *addEdge(B1, B2, Cfg);
  // Edges can have labels, indicating the type of control flow:
  Cfg[E] = std::make_tuple(ConditionalEdge::OnFalse, DirectEdge::IsDirect,
                           EdgeType::Fallthrough);
  // Symbolic expressions indicate that the value of a range of bytes depends on
  // the value of a symbol.
  BI->addSymbolicExpression(14, SymAddrConst{0, Sym1});
  // Finally, auxiliary data can be used to store additional information at the
  // IR and module level. A `gtirb::AuxData` object can store integers, strings,
  // GTIRB types such as `gtirb::Addr` and `gtirb::UUID`, and various containers
  // over these types. There are predefined AuxData schema for you to use, but
  // you can also use your own custom AuxData schema. Here is use of a
  // predefined schema, `gtirb::schema::Types`:
  M->addAuxData<gtirb::schema::Types>(
      {{D1->getUUID(), "string"}, {D2->getUUID(), "uleb128"}});
  //
  // ### Querying the IR
  //
  // Symbols can be looked up by address or name.  Any number of symbols can
  // share an address or name, so be prepared to deal with multiple results.
  for ([[maybe_unused]] const auto& Sym : M->findSymbols(Addr(2054))) {
    assert(Sym.getAddress() == Addr(2054));
    assert(Sym.getName() == "data2" || Sym.getName() == "duplicateReferent");
    assert(Sym.getReferent<DataBlock>() == nullptr ||
           Sym.getReferent<DataBlock>() == D2);
  }
  for ([[maybe_unused]] const auto& Sym : M->findSymbols("duplicateName")) {
    assert(Sym.getName() == "duplicateName");
    assert(Sym.getAddress() == Addr(2048) || Sym.getAddress() == Addr(4096));
  }
  // Use a symbol's referent (either a Block or DataObject) to get more
  // information about the object to which the symbol points.
  [[maybe_unused]] auto* Referent = Sym1->getReferent<DataBlock>();
  assert(Referent != nullptr);
  assert(Referent->getAddress() == Addr(2054));
  assert(Referent->getSize() == 2);
  assert(Referent->getByteInterval() == BI);
  assert(Referent->getOffset() == 6);
  // Alternatively, blocks can be looked up by an address contained within the
  // object. Any number of blocks may overlap and contain an address, so be
  // prepared to deal with multiple results.
  auto Blocks = M->findBlocksAt(Addr(2048), Addr(4096));
  assert(std::distance(Blocks.begin(), Blocks.end()) == 4);
  // The CFG uses
  // [boost::graph](https://www.boost.org/doc/libs/1_67_0/libs/graph/doc/).
  // GTIRB also provides a convenience function for iterating over blocks:
  for (const auto& B : blocks(Cfg)) {
    std::cout << "Block at address " << B.getAddress() << std::endl;
  }
  // To use boost::graph directly, you'll need to convert blocks into
  // `vertex_descriptor`s:
  auto [VerticesBegin, VerticesEnd] = boost::vertices(Cfg);
  for (const auto& Vertex :
       boost::make_iterator_range(VerticesBegin, VerticesEnd)) {
    if (Cfg[Vertex] == B2) {
      std::cout << "B2's vertex descriptor is: " << Vertex << std::endl;
    }
  }
  // And once you have those, you can use `edge_descriptor`s to look up labels
  // and the source/target blocks:
  auto [EdgesBegin, EdgesEnd] = boost::edges(Cfg);
  for (const auto& Edge : boost::make_iterator_range(EdgesBegin, EdgesEnd)) {
    auto V1 = boost::source(Edge, Cfg);
    auto V2 = boost::target(Edge, Cfg);
    std::cout << "Edge: " << Cfg[V1] << " => " << Cfg[V2] << std::endl;

    auto Label = *Cfg[Edge];
    std::cout << "Conditional? "
              << (std::get<ConditionalEdge>(Label) == ConditionalEdge::OnTrue)
              << std::endl;
    std::cout << "Direct? "
              << (std::get<DirectEdge>(Label) == DirectEdge::IsDirect)
              << std::endl;
    std::cout << "Fallthrough? "
              << (std::get<EdgeType>(Label) == EdgeType::Fallthrough)
              << std::endl;
  }
  // Aux data can be retrieved based on the schema and queried upon.
  auto* typesMap = M->getAuxData<gtirb::schema::Types>();
  if (typesMap) {
    for (const auto& [DataBlockID, BlockType] : *typesMap) {
      std::cout << "Data block with UUID " << DataBlockID << " is of type "
                << BlockType << "!";
    }
  }
  //
  // ### Serialization
  //
  // Serialize IR to a file with `gtirb::IR::save`.
  std::ofstream Out("path/to/file");
  Ir->save(Out);
  // Deserialize from a file with `gtirb::IR::load`.
  std::ifstream In("path/to/file");
  [[maybe_unused]] auto& NewIR = *IR::load(C, In);
  // END
  return 0;
}
