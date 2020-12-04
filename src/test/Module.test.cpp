//===- Module.test.cpp ------------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2020 GrammaTech, Inc.
//
//  This code is licensed under the MIT license. See the LICENSE file in the
//  project root for license terms.
//
//  This project is sponsored by the Office of Naval Research, One Liberty
//  Center, 875 N. Randolph Street, Arlington, VA 22203 under contract #
//  N68335-17-C-0700.  The content of the information does not necessarily
//  reflect the position or policy of the Government and no official
//  endorsement should be inferred.
//
//===----------------------------------------------------------------------===//
#include "SerializationTestHarness.hpp"
#include <gtirb/AuxData.hpp>
#include <gtirb/CodeBlock.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/DataBlock.hpp>
#include <gtirb/IR.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <gtirb/proto/Module.pb.h>
#include <algorithm>
#include <gtest/gtest.h>
#include <iterator>
#include <sstream>
#include <tuple>
#include <utility>

namespace gtirb {
namespace schema {

struct Foo {
  static constexpr const char* Name = "foo";
  typedef std::vector<int64_t> Type;
};

struct Bar {
  static constexpr const char* Name = "bar";
  typedef std::vector<char> Type;
};

struct AnTest {
  static constexpr const char* Name = "test";
  typedef uint32_t Type;
};

} // namespace schema
} // namespace gtirb

using namespace gtirb;
using namespace gtirb::schema;

void registerModuleTestAuxDataTypes() {
  AuxDataContainer::registerAuxDataType<Foo>();
  AuxDataContainer::registerAuxDataType<Bar>();
  AuxDataContainer::registerAuxDataType<AnTest>();
}

TEST(Unit_Module, compilationIteratorTypes) {
  static_assert(
      std::is_same_v<Module::code_block_iterator::reference, CodeBlock&>);
  static_assert(std::is_same_v<Module::const_code_block_iterator::reference,
                               const CodeBlock&>);
  static_assert(std::is_same_v<Module::code_block_subrange::iterator::reference,
                               CodeBlock&>);
  static_assert(
      std::is_same_v<Module::const_code_block_subrange::iterator::reference,
                     const CodeBlock&>);

  static_assert(
      std::is_same_v<Module::data_block_iterator::reference, DataBlock&>);
  static_assert(std::is_same_v<Module::const_data_block_iterator::reference,
                               const DataBlock&>);
  static_assert(std::is_same_v<Module::data_block_subrange::iterator::reference,
                               DataBlock&>);
  static_assert(
      std::is_same_v<Module::const_data_block_subrange::iterator::reference,
                     const DataBlock&>);

  static_assert(std::is_same_v<Module::section_iterator::reference, Section&>);
  static_assert(std::is_same_v<Module::const_section_iterator::reference,
                               const Section&>);
  static_assert(
      std::is_same_v<Module::section_name_iterator::reference, Section&>);
  static_assert(std::is_same_v<Module::const_section_name_iterator::reference,
                               const Section&>);
  static_assert(
      std::is_same_v<Module::section_subrange::iterator::reference, Section&>);
  static_assert(
      std::is_same_v<Module::const_section_subrange::iterator::reference,
                     const Section&>);

  static_assert(std::is_same_v<Module::symbolic_expression_iterator::reference,
                               ByteInterval::SymbolicExpressionElement>);
  static_assert(
      std::is_same_v<Module::const_symbolic_expression_iterator::reference,
                     ByteInterval::ConstSymbolicExpressionElement>);

  static_assert(std::is_same_v<Module::symbol_iterator::reference, Symbol&>);
  static_assert(
      std::is_same_v<Module::const_symbol_iterator::reference, const Symbol&>);
  static_assert(
      std::is_same_v<Module::symbol_addr_iterator::reference, Symbol&>);
  static_assert(std::is_same_v<Module::const_symbol_addr_iterator::reference,
                               const Symbol&>);
}

TEST(Unit_Module, noCopyMoveConstructors) {
  EXPECT_FALSE(std::is_copy_constructible_v<Module>);
  EXPECT_FALSE(std::is_move_constructible_v<Module>);
  EXPECT_FALSE(std::is_copy_assignable_v<Module>);
  EXPECT_FALSE(std::is_move_assignable_v<Module>);
}

static Context Ctx;

TEST(Unit_Module, ctor_0) { EXPECT_NE(Module::Create(Ctx, "M"), nullptr); }

TEST(Unit_Module, setBinaryPath) {
  const std::string StrPath("/home/gt/irb/foo");
  auto* M = Module::Create(Ctx, "M");

  M->setBinaryPath(StrPath);

  const auto& Path = M->getBinaryPath();
  EXPECT_EQ(StrPath, Path);
}

TEST(Unit_Module, getFileFormatDefault) {
  auto* M = Module::Create(Ctx, "M");
  EXPECT_EQ(gtirb::FileFormat::Undefined, M->getFileFormat());
}

TEST(Unit_Module, auxDataRanges) {
  auto* M = Module::Create(Ctx, "M");
  M->addAuxData<Foo>(std::vector<int64_t>{1, 2, 3});
  M->addAuxData<Bar>(std::vector<char>{'a', 'b', 'c'});

  auto A = M->aux_data();
  EXPECT_EQ(std::distance(A.begin(), A.end()), 2);
  // AuxDatas are sorted by range, but this is an implementation detail
  EXPECT_EQ(A.begin()->Key, Bar::Name);
  EXPECT_EQ((++A.begin())->Key, Foo::Name);
}

TEST(Unit_Module, setFileFormat) {
  auto* M = Module::Create(Ctx, "M");

  M->setFileFormat(gtirb::FileFormat::COFF);
  EXPECT_EQ(gtirb::FileFormat::COFF, M->getFileFormat());

  M->setFileFormat(gtirb::FileFormat::MACHO);
  EXPECT_EQ(gtirb::FileFormat::MACHO, M->getFileFormat());

  M->setFileFormat(gtirb::FileFormat::Undefined);
  EXPECT_EQ(gtirb::FileFormat::Undefined, M->getFileFormat());
}

TEST(Unit_Module, getRebaseDeltaDefault) {
  auto* M = Module::Create(Ctx, "M");
  EXPECT_EQ(int64_t{0}, M->getRebaseDelta());
}

TEST(Unit_Module, setRebaseDelta) {
  auto* M = Module::Create(Ctx, "M");

  EXPECT_FALSE(M->isRelocated());

  M->setRebaseDelta(1);
  EXPECT_EQ(int64_t{1}, M->getRebaseDelta());
  EXPECT_TRUE(M->isRelocated());

  M->setRebaseDelta(-1);
  EXPECT_EQ(int64_t{-1}, M->getRebaseDelta());
  EXPECT_TRUE(M->isRelocated());

  M->setRebaseDelta(std::numeric_limits<int64_t>::max());
  EXPECT_EQ(std::numeric_limits<int64_t>::max(), M->getRebaseDelta());
  EXPECT_TRUE(M->isRelocated());

  M->setRebaseDelta(std::numeric_limits<int64_t>::min());
  EXPECT_EQ(std::numeric_limits<int64_t>::min(), M->getRebaseDelta());
  EXPECT_TRUE(M->isRelocated());

  M->setRebaseDelta(std::numeric_limits<int64_t>::lowest());
  EXPECT_EQ(std::numeric_limits<int64_t>::lowest(), M->getRebaseDelta());
  EXPECT_TRUE(M->isRelocated());
}

TEST(Unit_Module, getPreferredAddrDefault) {
  auto* M = Module::Create(Ctx, "M");
  EXPECT_EQ(Addr{}, M->getPreferredAddr());
}

TEST(Unit_Module, getISA) {
  auto* M = Module::Create(Ctx, "M");

  EXPECT_EQ(gtirb::ISA::Undefined, M->getISA());

  M->setISA(gtirb::ISA::X64);
  EXPECT_EQ(gtirb::ISA::X64, M->getISA());
}

TEST(Unit_Module, setPreferredAddr) {
  auto* M = Module::Create(Ctx, "M");
  Addr Preferred{64};

  EXPECT_EQ(M->getPreferredAddr(), Addr(0));

  M->setPreferredAddr(Preferred);
  EXPECT_EQ(Preferred, M->getPreferredAddr());
}

TEST(Unit_Module, getSymbolSet) {
  auto* M = Module::Create(Ctx, "M");
  EXPECT_EQ(std::distance(M->symbols().begin(), M->symbols().end()), 0);
}

TEST(Unit_Module, getName) {
  auto* M = Module::Create(Ctx, "M");
  EXPECT_EQ(M->getName(), "M");
}

TEST(Unit_Module, setName) {
  auto* M = Module::Create(Ctx, "M");
  EXPECT_EQ(M->getName(), "M");

  M->setName("test");
  EXPECT_EQ(M->getName(), "test");
}

TEST(Unit_Module, sections) {
  auto* M = Module::Create(Ctx, "M");
  M->addSection(Ctx, "test");
  EXPECT_EQ(M->sections_begin()->getName(), "test");
  EXPECT_EQ(std::distance(M->sections_begin(), M->sections_end()), 1);
  EXPECT_EQ(
      std::distance(M->sections_by_name_begin(), M->sections_by_name_end()), 1);
}

TEST(Unit_Module, byteIntervals) {
  auto* M = Module::Create(Ctx, "M");
  auto* S1 = M->addSection(Ctx, "gamma");
  auto* S2 = M->addSection(Ctx, "beta");
  auto* S3 = M->addSection(Ctx, "alpha");
  auto* BI1 = S1->addByteInterval(Ctx, 10);
  auto* BI2 = S2->addByteInterval(Ctx, Addr(0), 10);
  auto* BI3 = S3->addByteInterval(Ctx, Addr(10), 10);

  // Create CodeBlocks and Symbols such that pointers do not change
  // monotonically with respect to address. This will help determine whether
  // CodeBlock and Symbol iteration is updated properly when ByteIntervals are
  // relocated.

  CodeBlock* CB[20];
  Symbol* Sym[20];
  for (int i = 0; i < 10; ++i) {
    int index = (i & 0x1) ? 4 - (i >> 1) : 5 + (i >> 1);
    CB[index] = BI2->addBlock<CodeBlock>(Ctx, index, 1);
    CB[index + 10] = BI3->addBlock<CodeBlock>(Ctx, index, 1);

    std::string name = "sym_";
    Sym[index] = M->addSymbol(Ctx, CB[index], name.append(1, 'a' + index));
    Sym[index + 10] =
        M->addSymbol(Ctx, CB[index + 10], name.append(1, 'a' + index + 10));
  }
  for (int i = 0; i < 20; ++i) {
    ASSERT_EQ(CB[i]->getAddress(), Addr(i));
    ASSERT_EQ(Sym[i]->getAddress(), Addr(i));
  }

  // Iteration should be in order of increasing start address.
  ASSERT_EQ(std::distance(M->byte_intervals_begin(), M->byte_intervals_end()),
            3);
  EXPECT_EQ(&*std::next(M->byte_intervals_begin(), 0), BI1);
  EXPECT_EQ(&*std::next(M->byte_intervals_begin(), 1), BI2);
  EXPECT_EQ(&*std::next(M->byte_intervals_begin(), 2), BI3);

  ASSERT_EQ(std::distance(M->sections_begin(), M->sections_end()), 3);
  EXPECT_EQ(&*std::next(M->sections_begin(), 0), S1);
  EXPECT_EQ(&*std::next(M->sections_begin(), 1), S2);
  EXPECT_EQ(&*std::next(M->sections_begin(), 2), S3);

  // CodeBlocks and symbols should be in order.
  ASSERT_EQ(std::distance(M->code_blocks_begin(), M->code_blocks_end()), 20);
  for (int i = 0; i < 20; ++i) {
    CodeBlock& CBi = *std::next(M->code_blocks_begin(), i);
    EXPECT_EQ(CBi.getAddress(), Addr(i));
    EXPECT_EQ(&CBi, CB[i]);
  }

  ASSERT_EQ(std::distance(M->symbols_by_addr_begin(), M->symbols_by_addr_end()),
            20);
  for (int i = 0; i < 20; ++i) {
    Symbol& S = *std::next(M->symbols_by_addr_begin(), i);
    EXPECT_EQ(S.getAddress(), Addr(i));
    EXPECT_EQ(&S, Sym[i]);
  }

  // Iteration should be alphabetical by section name.
  ASSERT_EQ(
      std::distance(M->sections_by_name_begin(), M->sections_by_name_end()), 3);
  EXPECT_EQ(&*std::next(M->sections_by_name_begin(), 0), S3);
  EXPECT_EQ(&*std::next(M->sections_by_name_begin(), 1), S2);
  EXPECT_EQ(&*std::next(M->sections_by_name_begin(), 2), S1);

  BI2->setAddress(Addr(20));

  // BI2 should now come last...
  ASSERT_EQ(std::distance(M->byte_intervals_begin(), M->byte_intervals_end()),
            3);
  EXPECT_EQ(&*std::next(M->byte_intervals_begin(), 0), BI1);
  EXPECT_EQ(&*std::next(M->byte_intervals_begin(), 1), BI3);
  EXPECT_EQ(&*std::next(M->byte_intervals_begin(), 2), BI2);

  ASSERT_EQ(std::distance(M->sections_begin(), M->sections_end()), 3);
  EXPECT_EQ(&*std::next(M->sections_begin(), 0), S1);
  EXPECT_EQ(&*std::next(M->sections_begin(), 1), S3);
  EXPECT_EQ(&*std::next(M->sections_begin(), 2), S2);

  // The first 10 CodeBlocks and Symbols (in BI2) should come after the second
  // 10 (in BI3).
  ASSERT_EQ(std::distance(M->code_blocks_begin(), M->code_blocks_end()), 20);
  for (int i = 0; i < 10; ++i) {
    CodeBlock& CBi = *std::next(M->code_blocks_begin(), i);
    EXPECT_EQ(CBi.getAddress(), Addr(i + 10));
    EXPECT_EQ(&CBi, CB[i + 10]);
  }
  for (int i = 0; i < 10; ++i) {
    CodeBlock& CBi = *std::next(M->code_blocks_begin(), i + 10);
    EXPECT_EQ(CBi.getAddress(), Addr(i + 20));
    EXPECT_EQ(&CBi, CB[i]);
  }

  ASSERT_EQ(std::distance(M->symbols_by_addr_begin(), M->symbols_by_addr_end()),
            20);
  for (int i = 0; i < 10; ++i) {
    Symbol& S = *std::next(M->symbols_by_addr_begin(), i);
    EXPECT_EQ(S.getAddress(), Addr(i + 10));
    EXPECT_EQ(&S, Sym[i + 10]);
  }
  for (int i = 0; i < 10; ++i) {
    Symbol& S = *std::next(M->symbols_by_addr_begin(), i + 10);
    EXPECT_EQ(S.getAddress(), Addr(i + 20));
    EXPECT_EQ(&S, Sym[i]);
  }

  // Iteration should be unaffected by changing addresses...
  ASSERT_EQ(
      std::distance(M->sections_by_name_begin(), M->sections_by_name_end()), 3);
  EXPECT_EQ(&*std::next(M->sections_by_name_begin(), 0), S3);
  EXPECT_EQ(&*std::next(M->sections_by_name_begin(), 1), S2);
  EXPECT_EQ(&*std::next(M->sections_by_name_begin(), 2), S1);
}

TEST(Unit_Module, getAddressAndSize) {
  auto* M = Module::Create(Ctx, "M");
  auto* S = M->addSection(Ctx, "test");
  auto* BI = S->addByteInterval(Ctx, 100);

  EXPECT_FALSE(M->getAddress());
  EXPECT_FALSE(M->getSize());

  BI->setAddress(Addr(200));

  ASSERT_TRUE(M->getAddress());
  EXPECT_EQ(M->getAddress(), Addr(200));
  ASSERT_TRUE(M->getSize());
  EXPECT_EQ(M->getSize(), 100);

  BI->setAddress(Addr(0));

  EXPECT_EQ(M->getAddress(), Addr(0));
  EXPECT_EQ(M->getSize(), 100);

  BI->setSize(15);

  EXPECT_EQ(M->getAddress(), Addr(0));
  EXPECT_EQ(M->getSize(), 15);
}

TEST(Unit_Module, findSections) {
  auto* M = Module::Create(Ctx, "M");
  auto* S = M->addSection(Ctx, "test");
  auto* BI = S->addByteInterval(Ctx, Addr(1), 123);

  {
    auto F = M->findSectionsOn(Addr(1));
    ASSERT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(F.begin()->getName(), "test");

    F = M->findSectionsOn(Addr(123));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);

    F = M->findSectionsOn(Addr(124));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 0);
  }

  BI->setAddress(std::nullopt);

  {
    auto F = M->findSectionsOn(Addr(1));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 0);
  }

  {
    auto F = M->findSections("test");
    ASSERT_NE(F, M->sections_by_name_end());
    EXPECT_EQ(F->getName(), "test");

    F = M->findSections("dummy");
    EXPECT_EQ(F, M->sections_by_name_end());
  }
}

TEST(Unit_Module, sectionNameOrder) {
  auto* M = Module::Create(Ctx, "M");
  auto* S1 = M->addSection(Ctx, "gamma");
  auto* S2 = M->addSection(Ctx, "beta");
  auto* S3 = M->addSection(Ctx, "alpha");

  ASSERT_EQ(
      std::distance(M->sections_by_name_begin(), M->sections_by_name_end()), 3);
  EXPECT_EQ(&*std::next(M->sections_by_name_begin(), 0), S3); // alpha
  EXPECT_EQ(&*std::next(M->sections_by_name_begin(), 1), S2); // beta
  EXPECT_EQ(&*std::next(M->sections_by_name_begin(), 2), S1); // gamma

  S2->setName("omega");
  ASSERT_EQ(
      std::distance(M->sections_by_name_begin(), M->sections_by_name_end()), 3);
  EXPECT_EQ(&*std::next(M->sections_by_name_begin(), 0), S3); // alpha
  EXPECT_EQ(&*std::next(M->sections_by_name_begin(), 1), S1); // gamma
  EXPECT_EQ(&*std::next(M->sections_by_name_begin(), 2), S2); // omega
}

TEST(Unit_Module, blocks) {
  auto I = IR::Create(Ctx);
  auto M = I->addModule(Ctx, "M");
  auto S = M->addSection(Ctx, "test");
  auto BI = S->addByteInterval(Ctx, Addr(1), 10);
  BI->addBlock<CodeBlock>(Ctx, 0, 10);

  ASSERT_EQ(std::distance(M->code_blocks_begin(), M->code_blocks_end()), 1);
  EXPECT_EQ(M->code_blocks_begin()->getAddress(), std::optional<Addr>(Addr(1)));

  auto F = blocks(M->getIR()->getCFG());
  ASSERT_EQ(std::distance(F.begin(), F.end()), 1);
  EXPECT_EQ(F.begin()->getAddress(), Addr(1));
}

TEST(Unit_Module, cfgNodes) {
  auto* I = IR::Create(Ctx);
  auto* M = I->addModule(Ctx, "M");
  auto S = M->addSection(Ctx, "test");
  auto BI = S->addByteInterval(Ctx, Addr(1), 10);
  auto* B = BI->addBlock<CodeBlock>(Ctx, 0, 10);
  auto* P = M->addProxyBlock(Ctx);

  EXPECT_EQ(std::distance(M->code_blocks_begin(), M->code_blocks_end()), 1);
  auto Nodes = nodes(M->getIR()->getCFG());
  EXPECT_EQ(std::distance(Nodes.begin(), Nodes.end()), 2);
  auto It = Nodes.begin();
  EXPECT_TRUE(&*It == B || &*It == P);
  ++It;
  EXPECT_NE(&*Nodes.begin(), &*It);
  EXPECT_TRUE(&*It == B || &*It == P);
}

TEST(Unit_Module, findBlock) {
  auto* M = Module::Create(Ctx, "M");
  auto* S = M->addSection(Ctx, "test");
  auto* BI = S->addByteInterval(Ctx, Addr(0), 30);
  auto* B1 = BI->addBlock<CodeBlock>(Ctx, 1, 20);
  auto* B2 = BI->addBlock<CodeBlock>(Ctx, 5, 10);

  {
    auto F = M->findCodeBlocksOn(Addr(0));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 0);

    F = M->findCodeBlocksOn(Addr(1));
    ASSERT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), B1);

    F = M->findCodeBlocksOn(Addr(5));
    ASSERT_EQ(std::distance(F.begin(), F.end()), 2);
    EXPECT_EQ(&*F.begin(), B1);
    EXPECT_EQ(&*++F.begin(), B2);

    F = M->findCodeBlocksOn(Addr(14));
    ASSERT_EQ(std::distance(F.begin(), F.end()), 2);
    EXPECT_EQ(&*F.begin(), B1);
    EXPECT_EQ(&*++F.begin(), B2);

    F = M->findCodeBlocksOn(Addr(15));
    ASSERT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), B1);

    F = M->findCodeBlocksOn(Addr(20));
    ASSERT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), B1);

    F = M->findCodeBlocksOn(Addr(21));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 0);
  }
}

TEST(Unit_Module, dataObjects) {
  auto* M = Module::Create(Ctx, "M");
  auto* S = M->addSection(Ctx, "test");
  auto* BI = S->addByteInterval(Ctx, Addr(1), 123);
  BI->addBlock<DataBlock>(Ctx, 0, 123);
  ASSERT_NE(M->data_blocks_begin(), M->data_blocks_end());
  EXPECT_EQ(M->data_blocks_begin()->getAddress(), Addr(1));
}

TEST(Unit_Module, findData) {
  auto* M = Module::Create(Ctx, "M");
  auto* S = M->addSection(Ctx, "test");
  auto* BI = S->addByteInterval(Ctx, Addr(0), 30);

  auto* D1 = BI->addBlock<DataBlock>(Ctx, 1, 10);
  auto* D2 = BI->addBlock<DataBlock>(Ctx, 5, 10);

  {
    auto F = M->findDataBlocksOn(Addr(0));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 0);

    F = M->findDataBlocksOn(Addr(1));
    ASSERT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), D1);

    F = M->findDataBlocksOn(Addr(5));
    ASSERT_EQ(std::distance(F.begin(), F.end()), 2);
    EXPECT_EQ(&*F.begin(), D1);
    EXPECT_EQ(&*(++F.begin()), D2);

    F = M->findDataBlocksOn(Addr(10));
    ASSERT_EQ(std::distance(F.begin(), F.end()), 2);
    EXPECT_EQ(&*F.begin(), D1);
    EXPECT_EQ(&*(++F.begin()), D2);

    F = M->findDataBlocksOn(Addr(11));
    ASSERT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), D2);

    F = M->findDataBlocksOn(Addr(14));
    ASSERT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), D2);

    F = M->findDataBlocksOn(Addr(15));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 0);
  }
}

TEST(Unit_Module, symbolIterationOrder) {
  auto* M = Module::Create(Ctx, "M");
  auto* S1 = M->addSymbol(Ctx, Addr(3), "foo");
  auto* S2 = M->addSymbol(Ctx, Addr(2), "bar");
  auto* S3 = M->addSymbol(Ctx, Addr(1), "foo");

  {
    auto F = M->symbols_by_name();
    EXPECT_EQ(std::distance(F.begin(), F.end()), 3);
    auto It = F.begin();
    // symbol_name_iterator returns values in name order but does not specify
    // order for symbols with the same name.
    EXPECT_EQ(&*It++, S2);
    EXPECT_EQ((std::set<Symbol*>{&*It++, &*It++}), (std::set<Symbol*>{S1, S3}));
  }
}

TEST(Unit_Module, findSymbols) {
  auto* M = Module::Create(Ctx, "M");
  auto* S = M->addSection(Ctx, "test");
  auto* BI = S->addByteInterval(Ctx, Addr(1), 1);
  auto* B = BI->addBlock<CodeBlock>(Ctx, 0, 1);

  auto* S1 = M->addSymbol(Ctx, Addr(1), "foo");
  auto* S2 = M->addSymbol(Ctx, B, "bar");
  auto* S3 = M->addSymbol(Ctx, Addr(2), "foo");
  auto* S4 = M->addSymbol(Ctx, B, "baz");

  {
    auto F = M->findSymbols("foo");
    EXPECT_EQ(std::distance(F.begin(), F.end()), 2);
    // Order of S1 and S2 is unspecified.
    EXPECT_EQ((std::set<Symbol*>{&*F.begin(), &*std::next(F.begin(), 1)}),
              (std::set<Symbol*>{S1, S3}));
  }

  {
    auto F = M->findSymbols("bar");
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), S2);
  }

  EXPECT_TRUE(M->findSymbols("notfound").empty());

  {
    auto F = M->findSymbols(Addr(1));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 3);
    // Order of S1, S2, and S4 is unspecified.
    EXPECT_EQ((std::set<Symbol*>{&*F.begin(), &*std::next(F.begin(), 1),
                                 &*std::next(F.begin(), 2)}),
              (std::set<Symbol*>{S1, S2, S4}));
  }

  {
    auto F = M->findSymbols(Addr(2));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), S3);
  }

  EXPECT_TRUE(M->findSymbols(Addr(3)).empty());

  {
    auto F = M->findSymbols(Addr(0), Addr(2));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 3);
    // Order of S1, S2, and S4 is unspecified.
    EXPECT_EQ((std::set<Symbol*>{&*F.begin(), &*std::next(F.begin(), 1),
                                 &*std::next(F.begin(), 2)}),
              (std::set<Symbol*>{S1, S2, S4}));
  }

  {
    auto F = M->findSymbols(Addr(0), Addr(5));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 4);
    // Order of S1, S2, and S4 is unspecified. All three should be returned
    // before S3.
    EXPECT_EQ((std::set<Symbol*>{&*F.begin(), &*std::next(F.begin(), 1),
                                 &*std::next(F.begin(), 2)}),
              (std::set<Symbol*>{S1, S2, S4}));
    EXPECT_EQ(&*std::next(F.begin(), 3), S3);
  }

  {
    auto F = M->findSymbols(Addr(10), Addr(25));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 0);
  }

  {
    auto F = M->findSymbols(*B);
    EXPECT_EQ(std::distance(F.begin(), F.end()), 2);
    // Order of S2 and S4 is unspecified.
    EXPECT_EQ((std::set<Symbol*>{&*F.begin(), &*std::next(F.begin(), 1)}),
              (std::set<Symbol*>{S2, S4}));
  }
}

TEST(Unit_Module, symbolWithoutAddr) {
  auto* M = Module::Create(Ctx, "M");
  M->addSymbol(Ctx, "test");
  EXPECT_EQ(M->findSymbols("test").begin()->getName(), "test");
}

TEST(Unit_Module, renameSymbol) {
  auto* M = Module::Create(Ctx, "M");
  auto* S1 = M->addSymbol(Ctx, "foo");
  auto* S2 = M->addSymbol(Ctx, Addr(1), "bar");
  auto* S3 = M->addSymbol(Ctx, Addr(2), "bar");

  S1->setName("test1");
  S2->setName("test2");

  {
    auto F = M->findSymbols("foo");
    EXPECT_EQ(std::distance(F.begin(), F.end()), 0);

    F = M->findSymbols("bar");
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), S3);

    F = M->findSymbols("test1");
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), S1);

    F = M->findSymbols("test2");
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), S2);
  }
  {
    auto F = M->findSymbols(Addr(1));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), S2);

    F = M->findSymbols(Addr(2));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), S3);
  }
}

TEST(Unit_Module, setReferent) {
  auto* M = Module::Create(Ctx, "M");
  auto* S = M->addSection(Ctx, "test");
  auto* BI = S->addByteInterval(Ctx, Addr(0), 5);

  auto* B1 = BI->addBlock<CodeBlock>(Ctx, 1, 1);
  auto* B2 = BI->addBlock<CodeBlock>(Ctx, 2, 1);
  auto* B3 = BI->addBlock<CodeBlock>(Ctx, 3, 1);
  auto* B4 = BI->addBlock<CodeBlock>(Ctx, 4, 1);
  auto* B5 = BI->addBlock<CodeBlock>(Ctx, 5, 1);
  auto* S1 = M->addSymbol(Ctx, "foo");
  auto* S2 = M->addSymbol(Ctx, B1, "bar");
  auto* S3 = M->addSymbol(Ctx, B1, "foo");
  auto* S4 = M->addSymbol(Ctx, B2, "bar");

  S1->setReferent(B3);
  S2->setReferent(B4);
  S4->setReferent(B5);

  {
    auto F = M->findSymbols("foo");
    EXPECT_EQ(std::distance(F.begin(), F.end()), 2);
    EXPECT_EQ(&*F.begin(), S1);
    EXPECT_EQ(&*(++F.begin()), S3);

    F = M->findSymbols("bar");
    EXPECT_EQ(std::distance(F.begin(), F.end()), 2);
    EXPECT_EQ(&*F.begin(), S2);
    EXPECT_EQ(&*(++F.begin()), S4);
  }

  {
    auto F = M->findSymbols(Addr(1));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), S3);

    F = M->findSymbols(Addr(2));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 0);

    F = M->findSymbols(Addr(3));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), S1);

    F = M->findSymbols(Addr(4));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), S2);

    F = M->findSymbols(Addr(5));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), S4);
  }
}

TEST(Unit_Module, setSymbolAddress) {
  auto* M = Module::Create(Ctx, "M");
  auto* S = M->addSection(Ctx, "test");
  auto* BI = S->addByteInterval(Ctx, Addr(0), 5);

  auto* B1 = BI->addBlock<CodeBlock>(Ctx, 1, 1);
  auto* S1 = M->addSymbol(Ctx, "foo");
  auto* S2 = M->addSymbol(Ctx, B1, "bar");
  auto* S3 = M->addSymbol(Ctx, B1, "bar");

  S1->setAddress(Addr(2));
  S2->setAddress(Addr(3));

  {
    auto F = M->findSymbols("bar");
    EXPECT_EQ(std::distance(F.begin(), F.end()), 2);
    EXPECT_EQ(&*F.begin(), S2);
    EXPECT_EQ(&*(++F.begin()), S3);
  }

  {
    auto F = M->findSymbols(Addr(1));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), S3);

    F = M->findSymbols(Addr(2));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), S1);

    F = M->findSymbols(Addr(3));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), S2);
  }
}

TEST(Unit_Module, symbolicExpressions) {
  auto* M = Module::Create(Ctx, "M");
  auto* S = M->addSection(Ctx, "test");
  auto* BI = S->addByteInterval(Ctx, Addr(1), 123);
  auto* Sym = M->addSymbol(Ctx, "test");

  BI->addSymbolicExpression(0, SymAddrConst{0, Sym});
  EXPECT_EQ(std::distance(M->symbolic_expressions_begin(),
                          M->symbolic_expressions_end()),
            1);
}

TEST(Unit_Module, findSymbolicExpressionsAts) {
  auto* M = Module::Create(Ctx, "M");
  auto* S = M->addSection(Ctx, "test");
  auto* BI = S->addByteInterval(Ctx, Addr(0), 10);

  auto* S1 = M->addSymbol(Ctx, Addr(1), "foo");
  auto* S2 = M->addSymbol(Ctx, Addr(5), "bar");

  BI->addSymbolicExpression(1, SymAddrConst{0, S1});
  BI->addSymbolicExpression(5, SymAddrConst{0, S2});

  {
    auto F = M->findSymbolicExpressionsAt(Addr(1), Addr(5));
    ASSERT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(std::get<SymAddrConst>(F.begin()->getSymbolicExpression()).Sym,
              S1);
  }

  {
    auto F = M->findSymbolicExpressionsAt(Addr(1), Addr(6));
    ASSERT_EQ(std::distance(F.begin(), F.end()), 2);
    EXPECT_EQ(std::get<SymAddrConst>(F.begin()->getSymbolicExpression()).Sym,
              S1);
    EXPECT_EQ(
        std::get<SymAddrConst>((++F.begin())->getSymbolicExpression()).Sym, S2);
  }

  {
    auto F = M->findSymbolicExpressionsAt(Addr(1), Addr(3));
    ASSERT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(std::get<SymAddrConst>(F.begin()->getSymbolicExpression()).Sym,
              S1);
  }

  {
    auto F = M->findSymbolicExpressionsAt(Addr(6), Addr(50));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 0);
  }
}

TEST(Unit_Module, protobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  std::stringstream ss;

  UUID BlockID, DataID, ProxyID, SectionID;
  size_t WhichSymbolic;

  {
    Context InnerCtx;
    IR* OI = IR::Create(InnerCtx);
    Module* Original = OI->addModule(InnerCtx, "module");
    Original->setBinaryPath("test");
    Original->setPreferredAddr(Addr(3));
    Original->setRebaseDelta(4);
    Original->setFileFormat(FileFormat::ELF);
    Original->setISA(ISA::X64);
    Original->addAuxData<AnTest>(0);
    Symbol* Sym = Original->addSymbol(InnerCtx, Addr(1), "name1");
    Original->addSymbol(InnerCtx, Addr(2), "name1");
    Original->addSymbol(InnerCtx, Addr(1), "name3");
    auto S = Original->addSection(InnerCtx, "test");
    auto BI = S->addByteInterval(InnerCtx, Addr(1), 2);
    BI->addBlock<CodeBlock>(InnerCtx, 0, 2);
    BI->addBlock<DataBlock>(InnerCtx, 0, 2);
    auto* P = Original->addProxyBlock(InnerCtx);
    BI->addSymbolicExpression<SymAddrConst>(7, 0, Sym);
    BlockID = blocks(Original->getIR()->getCFG()).begin()->getUUID();
    DataID = Original->data_blocks_begin()->getUUID();
    ProxyID = P->getUUID();
    SectionID = Original->sections_begin()->getUUID();
    WhichSymbolic = Original->symbolic_expressions_begin()->getOffset();

    STH::save(*Original, ss);
  }

  Module* Result = STH::load<Module>(Ctx, ss);
  IR::Create(Ctx)->addModule(Result);

  EXPECT_EQ(Result->getBinaryPath(), "test");
  EXPECT_EQ(Result->getPreferredAddr(), Addr(3));
  EXPECT_EQ(Result->getRebaseDelta(), 4);
  EXPECT_EQ(Result->getFileFormat(), FileFormat::ELF);
  EXPECT_EQ(Result->getISA(), ISA::X64);
  EXPECT_EQ(Result->getName(), "module");

  // Make sure all symbols are present despite repeated names and addresses.
  EXPECT_EQ(std::distance(Result->symbols_begin(), Result->symbols_end()), 3);
  {
    auto Found = Result->findSymbols("name1");
    EXPECT_EQ(distance(Found.begin(), Found.end()), 2);
  }
  {
    auto Found = Result->findSymbols(Addr(1));
    EXPECT_EQ(distance(Found.begin(), Found.end()), 2);
  }

  // Make sure various collections and node members are serialized, but
  // don't check in detail as they have their own unit tests.
  EXPECT_EQ(Result->getAuxDataSize(), 1);
  EXPECT_NE(Result->getAuxData<AnTest>(), nullptr);

  ASSERT_EQ(num_vertices(Result->getIR()->getCFG()), 2);
  {
    auto Nodes = nodes(Result->getIR()->getCFG());
    auto It = Nodes.begin();
    EXPECT_TRUE(&*It);
    EXPECT_TRUE(It->getUUID() == BlockID || It->getUUID() == ProxyID);
    ++It;
    EXPECT_TRUE(&*It);
    EXPECT_NE(Nodes.begin()->getUUID(), It->getUUID());
    EXPECT_TRUE(It->getUUID() == BlockID || It->getUUID() == ProxyID);
  }

  ASSERT_EQ(
      std::distance(Result->code_blocks_begin(), Result->code_blocks_end()), 1);
  EXPECT_EQ(Result->code_blocks_begin()->getUUID(), BlockID);
  ASSERT_EQ(
      std::distance(Result->data_blocks_begin(), Result->data_blocks_end()), 1);
  EXPECT_EQ(Result->data_blocks_begin()->getUUID(), DataID);
  ASSERT_EQ(
      std::distance(Result->proxy_blocks_begin(), Result->proxy_blocks_end()),
      1);
  EXPECT_EQ(Result->proxy_blocks_begin()->getUUID(), ProxyID);
  EXPECT_EQ(Result->proxy_blocks_begin()->getModule(), Result);

  ASSERT_EQ(std::distance(Result->sections_begin(), Result->sections_end()), 1);
  EXPECT_EQ(Result->sections_begin()->getUUID(), SectionID);

  ASSERT_EQ(std::distance(Result->symbolic_expressions_begin(),
                          Result->symbolic_expressions_end()),
            1);
  EXPECT_EQ(Result->symbolic_expressions_begin()->getOffset(), WhichSymbolic);
}

TEST(Unit_Module, protobufNodePointers) {
  // Ensure that when we cannot load an object that does not exist in the
  // serialized form that we fail to load the GTIRB again because it is wrong.
  using STH = gtirb::SerializationTestHarness;
  std::stringstream ss;

  {
    Context InnerCtx;
    auto* Original = Module::Create(InnerCtx, "M");
    auto* S = Original->addSection(InnerCtx, "test");
    auto* BI = S->addByteInterval(InnerCtx, Addr(1), 2);
    auto* Data = BI->addBlock<DataBlock>(InnerCtx, 0, 0);
    auto* DataSym = Original->addSymbol(InnerCtx, Data, "data");

    // Not part of IR
    auto* DanglingData = DataBlock::Create(InnerCtx, 0);
    Original->addSymbol(InnerCtx, DanglingData, "dangling");

    auto* Code = BI->addBlock<CodeBlock>(InnerCtx, 0, 2);
    Original->addSymbol(InnerCtx, Code, "code");
    BI->addSymbolicExpression<SymAddrConst>(2, 0, DataSym);

    // Not part of IR
    auto* DanglingSym = Symbol::Create(InnerCtx, Addr(1), "foo");
    BI->addSymbolicExpression<SymAddrConst>(3, 0, DanglingSym);

    STH::save(*Original, ss);
  }

  auto* Result = STH::load<Module>(Ctx, ss);
  EXPECT_EQ(Result, nullptr);
}

TEST(Unit_Module, removeNodes) {
  auto* M = Module::Create(Ctx, "M");
  auto* S = M->addSection(Ctx, "test");
  auto* BI = S->addByteInterval(Ctx, Addr(0), 0);
  BI->addBlock<CodeBlock>(Ctx, 0, 0);
  BI->addBlock<DataBlock>(Ctx, 0, 0);
  auto* P = M->addProxyBlock(Ctx);
  auto* Sym = M->addSymbol(Ctx, "test");

  EXPECT_EQ(std::distance(M->sections_begin(), M->sections_end()), 1);
  EXPECT_EQ(std::distance(M->byte_intervals_begin(), M->byte_intervals_end()),
            1);
  EXPECT_EQ(std::distance(M->code_blocks_begin(), M->code_blocks_end()), 1);
  EXPECT_EQ(std::distance(M->data_blocks_begin(), M->data_blocks_end()), 1);
  EXPECT_EQ(std::distance(M->proxy_blocks_begin(), M->proxy_blocks_end()), 1);
  EXPECT_EQ(std::distance(M->symbols_begin(), M->symbols_end()), 1);

  M->removeSection(S);

  EXPECT_EQ(std::distance(M->sections_begin(), M->sections_end()), 0);
  EXPECT_EQ(std::distance(M->byte_intervals_begin(), M->byte_intervals_end()),
            0);
  EXPECT_EQ(std::distance(M->code_blocks_begin(), M->code_blocks_end()), 0);
  EXPECT_EQ(std::distance(M->data_blocks_begin(), M->data_blocks_end()), 0);
  EXPECT_EQ(std::distance(M->proxy_blocks_begin(), M->proxy_blocks_end()), 1);
  EXPECT_EQ(std::distance(M->symbols_begin(), M->symbols_end()), 1);

  M->removeProxyBlock(P);

  EXPECT_EQ(std::distance(M->sections_begin(), M->sections_end()), 0);
  EXPECT_EQ(std::distance(M->byte_intervals_begin(), M->byte_intervals_end()),
            0);
  EXPECT_EQ(std::distance(M->code_blocks_begin(), M->code_blocks_end()), 0);
  EXPECT_EQ(std::distance(M->data_blocks_begin(), M->data_blocks_end()), 0);
  EXPECT_EQ(std::distance(M->proxy_blocks_begin(), M->proxy_blocks_end()), 0);
  EXPECT_EQ(std::distance(M->symbols_begin(), M->symbols_end()), 1);

  M->removeSymbol(Sym);

  EXPECT_EQ(std::distance(M->sections_begin(), M->sections_end()), 0);
  EXPECT_EQ(std::distance(M->byte_intervals_begin(), M->byte_intervals_end()),
            0);
  EXPECT_EQ(std::distance(M->code_blocks_begin(), M->code_blocks_end()), 0);
  EXPECT_EQ(std::distance(M->data_blocks_begin(), M->data_blocks_end()), 0);
  EXPECT_EQ(std::distance(M->proxy_blocks_begin(), M->proxy_blocks_end()), 0);
  EXPECT_EQ(std::distance(M->symbols_begin(), M->symbols_end()), 0);
}

TEST(Unit_Module, removeInvalidSection) {
  auto* M1 = Module::Create(Ctx, "M1");
  auto* M2 = Module::Create(Ctx, "M2");
  auto* S1 = M1->addSection(Ctx, "S1");
  S1->addByteInterval(Ctx, Addr(0), 10);
  auto* S2 = M2->addSection(Ctx, "S2");
  S2->addByteInterval(Ctx, Addr(0), 10);

  EXPECT_FALSE(M1->sections().empty());
  EXPECT_FALSE(M1->findSectionsOn(Addr(5)).empty());
  EXPECT_FALSE(M2->sections().empty());
  EXPECT_FALSE(M2->findSectionsOn(Addr(5)).empty());

  // Removing a section from a Module that isn't its parent should have no
  // effect.
  M1->removeSection(S2);

  EXPECT_FALSE(M1->sections().empty());
  EXPECT_FALSE(M1->findSectionsOn(Addr(5)).empty());
  EXPECT_FALSE(M2->sections().empty());
  EXPECT_FALSE(M2->findSectionsOn(Addr(5)).empty());
}

TEST(Unit_Module, mutateBlocksWithSymbols) {
  auto* M = Module::Create(Ctx, "M");
  auto* S = M->addSection(Ctx, "test");
  auto* BIC = S->addByteInterval(Ctx, Addr(0), 10);
  auto* BID = S->addByteInterval(Ctx, Addr(10), 10);
  auto* CB = BIC->addBlock<CodeBlock>(Ctx, 1, 2);
  auto* DB = BID->addBlock<DataBlock>(Ctx, 3, 4);
  auto* SymC = M->addSymbol(Ctx, CB, "code");
  auto* SymD = M->addSymbol(Ctx, DB, "data");
  Module::symbol_addr_range Range;

  Range = M->findSymbols(Addr(1));
  EXPECT_EQ(std::distance(Range.begin(), Range.end()), 1);
  EXPECT_EQ(&Range.front(), SymC);

  Range = M->findSymbols(Addr(13));
  EXPECT_EQ(std::distance(Range.begin(), Range.end()), 1);
  EXPECT_EQ(&Range.front(), SymD);

  Range = M->findSymbols(Addr(21));
  EXPECT_EQ(std::distance(Range.begin(), Range.end()), 0);

  BIC->setAddress(Addr(20));

  Range = M->findSymbols(Addr(1));
  EXPECT_EQ(std::distance(Range.begin(), Range.end()), 0);

  Range = M->findSymbols(Addr(13));
  EXPECT_EQ(std::distance(Range.begin(), Range.end()), 1);
  EXPECT_EQ(&Range.front(), SymD);

  Range = M->findSymbols(Addr(21));
  EXPECT_EQ(std::distance(Range.begin(), Range.end()), 1);
  EXPECT_EQ(&Range.front(), SymC);
}

TEST(Unit_Module, addBlocksWithSymbols) {
  auto* M = Module::Create(Ctx, "M");
  auto* S = M->addSection(Ctx, "test");
  auto* BI1 = S->addByteInterval(Ctx, Addr(0), 10);
  auto* BI2 = S->addByteInterval(Ctx, Addr(10), 10);
  auto* BI3 = S->addByteInterval(Ctx, Addr(20), 10);
  auto* BC = BI1->addBlock<CodeBlock>(Ctx, 1, 2);
  auto* BD = BI2->addBlock<DataBlock>(Ctx, 1, 2);
  auto* SymC = M->addSymbol(Ctx, BC, "code");
  auto* SymD = M->addSymbol(Ctx, BD, "data");

  Module::symbol_addr_range Range;

  Range = M->findSymbols(Addr(1));
  EXPECT_EQ(std::distance(Range.begin(), Range.end()), 1);
  EXPECT_EQ(&Range.front(), SymC);

  Range = M->findSymbols(Addr(11));
  EXPECT_EQ(std::distance(Range.begin(), Range.end()), 1);
  EXPECT_EQ(&Range.front(), SymD);

  Range = M->findSymbols(Addr(21));
  EXPECT_EQ(std::distance(Range.begin(), Range.end()), 0);

  BI3->addBlock(1, BC);

  Range = M->findSymbols(Addr(1));
  EXPECT_EQ(std::distance(Range.begin(), Range.end()), 0);

  Range = M->findSymbols(Addr(11));
  EXPECT_EQ(std::distance(Range.begin(), Range.end()), 1);
  EXPECT_EQ(&Range.front(), SymD);

  Range = M->findSymbols(Addr(21));
  EXPECT_EQ(std::distance(Range.begin(), Range.end()), 1);
  EXPECT_EQ(&Range.front(), SymC);
}
