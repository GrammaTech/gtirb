//===- Module.test.cpp ------------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2018 GrammaTech, Inc.
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
#include <gtirb/AuxData.hpp>
#include <gtirb/CodeBlock.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/DataBlock.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <proto/Module.pb.h>
#include <algorithm>
#include <gtest/gtest.h>
#include <iterator>
#include <tuple>
#include <utility>

using namespace gtirb;

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
  // Actually calling the constructor and assignment operator tends to produce
  // more informative error messages than std::is_constructible and
  // std::is_assignable.
  {
    Module::code_block_iterator it;
    Module::const_code_block_iterator cit(it);
    cit = it;
  }

  static_assert(
      std::is_same_v<Module::data_block_iterator::reference, DataBlock&>);
  static_assert(std::is_same_v<Module::const_data_block_iterator::reference,
                               const DataBlock&>);
  static_assert(std::is_same_v<Module::data_block_subrange::iterator::reference,
                               DataBlock&>);
  static_assert(
      std::is_same_v<Module::const_data_block_subrange::iterator::reference,
                     const DataBlock&>);
  {
    Module::data_block_iterator it;
    Module::const_data_block_iterator cit(it);
    cit = it;
  }

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

  {
    Module::section_iterator it;
    Module::const_section_iterator cit(it);
    cit = it;
  }

  {
    Module::section_name_iterator it;
    Module::const_section_name_iterator cit(it);
    cit = it;
  }

  // There are no non-const symbolic_expr_iterators...
  static_assert(
      std::is_same_v<Module::const_symbolic_expression_iterator::reference,
                     const SymbolicExpression&>);
  // There are, however, non-const subranges,
  // thanks to differences in internal storage.
  static_assert(
      std::is_same_v<Module::symbolic_expression_subrange::iterator::reference,
                     SymbolicExpression&>);
  static_assert(std::is_same_v<
                Module::const_symbolic_expression_subrange::iterator::reference,
                const SymbolicExpression&>);

  static_assert(std::is_same_v<Module::symbol_iterator::reference, Symbol&>);
  static_assert(
      std::is_same_v<Module::const_symbol_iterator::reference, const Symbol&>);
  static_assert(
      std::is_same_v<Module::symbol_addr_iterator::reference, Symbol&>);
  static_assert(std::is_same_v<Module::const_symbol_addr_iterator::reference,
                               const Symbol&>);

  {
    Module::symbol_iterator it;
    Module::const_symbol_iterator cit(it);
    cit = it;
  }
}

static Context Ctx;

TEST(Unit_Module, ctor_0) { EXPECT_NE(Module::Create(Ctx), nullptr); }

TEST(Unit_Module, setBinaryPath) {
  const std::string StrPath("/home/gt/irb/foo");
  auto* M = Module::Create(Ctx);

  M->setBinaryPath(StrPath);

  auto Path = M->getBinaryPath();
  EXPECT_EQ(StrPath, Path);
}

TEST(Unit_Module, getFileFormatDefault) {
  auto* M = Module::Create(Ctx);
  EXPECT_EQ(gtirb::FileFormat::Undefined, M->getFileFormat());
}

TEST(Unit_Module, auxDataRanges) {
  Module* Ml = Module::Create(Ctx);
  Ml->addAuxData("foo", std::vector<int64_t>{1, 2, 3});
  Ml->addAuxData("bar", std::vector<char>{'a', 'b', 'c'});

  auto A = Ml->aux_data();
  EXPECT_EQ(std::distance(A.begin(), A.end()), 2);
  // AuxDatas are sorted by range, but this is an implementation detail
  EXPECT_EQ(A.begin()->first, "bar");
  EXPECT_EQ((++A.begin())->first, "foo");
}

TEST(Unit_Module, setFileFormat) {
  auto* M = Module::Create(Ctx);

  M->setFileFormat(gtirb::FileFormat::COFF);
  EXPECT_EQ(gtirb::FileFormat::COFF, M->getFileFormat());

  M->setFileFormat(gtirb::FileFormat::MACHO);
  EXPECT_EQ(gtirb::FileFormat::MACHO, M->getFileFormat());

  M->setFileFormat(gtirb::FileFormat::Undefined);
  EXPECT_EQ(gtirb::FileFormat::Undefined, M->getFileFormat());
}

TEST(Unit_Module, getRebaseDeltaDefault) {
  auto* M = Module::Create(Ctx);
  EXPECT_EQ(int64_t{0}, M->getRebaseDelta());
}

TEST(Unit_Module, setRebaseDelta) {
  auto* M = Module::Create(Ctx);

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
  auto* M = Module::Create(Ctx);
  EXPECT_EQ(Addr{}, M->getPreferredAddr());
}

TEST(Unit_Module, getISAID) {
  auto* M = Module::Create(Ctx);

  EXPECT_EQ(gtirb::ISAID::Undefined, M->getISAID());

  M->setISAID(gtirb::ISAID::X64);
  EXPECT_EQ(gtirb::ISAID::X64, M->getISAID());
}

TEST(Unit_Module, setPreferredAddr) {
  auto* M = Module::Create(Ctx);
  Addr Preferred{64};

  EXPECT_EQ(M->getPreferredAddr(), Addr(0));

  M->setPreferredAddr(Preferred);
  EXPECT_EQ(Preferred, M->getPreferredAddr());
}

TEST(Unit_Module, getSymbolSet) {
  auto* M = Module::Create(Ctx);
  EXPECT_EQ(std::distance(M->symbols().begin(), M->symbols().end()), 0);
}

TEST(Unit_Module, getName) {
  auto* M = Module::Create(Ctx);
  EXPECT_TRUE(M->getName().empty());
}

TEST(Unit_Module, sections) {
  auto* M = Module::Create(Ctx);
  M->addSection(Section::Create(Ctx, "test"));
  EXPECT_EQ(M->section_begin()->getName(), "test");
  EXPECT_EQ(std::distance(M->section_begin(), M->section_end()), 1);
  EXPECT_EQ(std::distance(M->section_by_name_begin(), M->section_by_name_end()),
            1);
}

TEST(Unit_Module, findSection) {
  auto* M = Module::Create(Ctx);
  auto* S = Section::Create(Ctx, "test");
  M->addSection(S);
  auto* BI = ByteInterval::Create(Ctx, Addr(1), 123);
  M->addByteInterval(S, BI);

  {
    auto F = M->findSection(Addr(1));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(F.begin()->getName(), "test");

    F = M->findSection(Addr(123));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);

    F = M->findSection(Addr(124));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 0);
  }

  {
    auto F = M->findSection("test");
    EXPECT_NE(F, M->section_by_name_end());
    EXPECT_EQ(F->getName(), "test");

    F = M->findSection("dummy");
    EXPECT_EQ(F, M->section_by_name_end());
  }
}

TEST(Unit_Module, blocks) {
  auto M = Module::Create(Ctx);
  auto S = Section::Create(Ctx, "test");
  M->addSection(S);
  auto BI = ByteInterval::Create(Ctx, Addr(1), 10);
  M->addByteInterval(S, BI);
  auto B = CodeBlock::Create(Ctx, 10);
  M->addCodeBlock(BI, B);

  EXPECT_EQ(std::distance(M->code_blocks_begin(), M->code_blocks_end()), 1);
  EXPECT_EQ(BI->getAddress(&*M->code_blocks_begin()),
            std::optional<Addr>(Addr(1)));

  auto F = blocks(M->getCFG());
  EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
  EXPECT_EQ(BI->getAddress(&*F.begin()), Addr(1));
}

TEST(Unit_Module, cfgNodes) {
  auto* M = Module::Create(Ctx);
  auto S = Section::Create(Ctx, "test");
  M->addSection(S);
  auto BI = ByteInterval::Create(Ctx, Addr(1), 10);
  M->addByteInterval(S, BI);
  auto* B = CodeBlock::Create(Ctx, 10);
  M->addCodeBlock(BI, B);
  auto* P = ProxyBlock::Create(Ctx);
  M->addProxyBlock(P);

  EXPECT_EQ(std::distance(M->code_blocks_begin(), M->code_blocks_end()), 1);
  auto Nodes = nodes(M->getCFG());
  EXPECT_EQ(std::distance(Nodes.begin(), Nodes.end()), 2);
  auto It = Nodes.begin();
  EXPECT_TRUE(&*It == B || &*It == P);
  ++It;
  EXPECT_NE(&*Nodes.begin(), &*It);
  EXPECT_TRUE(&*It == B || &*It == P);
}

TEST(Unit_Module, findBlock) {
  auto M = Module::Create(Ctx);
  auto S = Section::Create(Ctx, "test");
  M->addSection(S);
  auto BI = ByteInterval::Create(Ctx, Addr(0), 30);
  M->addByteInterval(S, BI);
  auto* B1 = CodeBlock::Create(Ctx, 20);
  M->addCodeBlock(BI, 1, B1);
  auto* B2 = CodeBlock::Create(Ctx, 10);
  M->addCodeBlock(BI, 5, B2);

  {
    auto F = M->findCodeBlock(Addr(0));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 0);

    F = M->findCodeBlock(Addr(1));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), B1);

    F = M->findCodeBlock(Addr(5));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 2);
    EXPECT_EQ(&*F.begin(), B1);
    EXPECT_EQ(&*++F.begin(), B2);

    F = M->findCodeBlock(Addr(14));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 2);
    EXPECT_EQ(&*F.begin(), B1);
    EXPECT_EQ(&*++F.begin(), B2);

    F = M->findCodeBlock(Addr(15));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), B1);

    F = M->findCodeBlock(Addr(20));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), B1);

    F = M->findCodeBlock(Addr(21));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 0);
  }
}

TEST(Unit_Module, dataObjects) {
  auto M = Module::Create(Ctx);
  auto S = Section::Create(Ctx, "test");
  M->addSection(S);
  auto BI = ByteInterval::Create(Ctx, Addr(1), 123);
  M->addByteInterval(S, BI);
  M->addDataBlock(BI, 0, DataBlock::Create(Ctx));
  EXPECT_EQ(BI->getAddress(&*M->data_blocks_begin()), Addr(1));
}

TEST(Unit_Module, findData) {
  auto M = Module::Create(Ctx);
  auto S = Section::Create(Ctx, "test");
  M->addSection(S);
  auto BI = ByteInterval::Create(Ctx, Addr(0), 30);
  M->addByteInterval(S, BI);

  auto* D1 = DataBlock::Create(Ctx);
  auto* D2 = DataBlock::Create(Ctx);
  M->addDataBlock(BI, 1, D1);
  M->addDataBlock(BI, 5, D2);

  {
    auto F = M->findDataBlock(Addr(0));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 0);

    F = M->findDataBlock(Addr(1));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), D1);

    F = M->findDataBlock(Addr(5));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 2);
    EXPECT_EQ(&*F.begin(), D1);
    EXPECT_EQ(&*(++F.begin()), D2);

    F = M->findDataBlock(Addr(14));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 2);
    EXPECT_EQ(&*F.begin(), D1);
    EXPECT_EQ(&*(++F.begin()), D2);

    F = M->findDataBlock(Addr(15));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), D1);

    F = M->findDataBlock(Addr(20));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), D1);

    F = M->findDataBlock(Addr(21));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 0);
  }
}

TEST(Unit_Module, symbolIterationOrder) {
  auto* M = Module::Create(Ctx);
  auto* S1 = emplaceSymbol(*M, Ctx, Addr(3), "foo");
  auto* S2 = emplaceSymbol(*M, Ctx, Addr(2), "bar");
  auto* S3 = emplaceSymbol(*M, Ctx, Addr(1), "foo");

  {
    auto F = M->symbols();
    EXPECT_EQ(std::distance(F.begin(), F.end()), 3);
    auto It = F.begin();
    // symbol_iterator returns values in name order but does not specify order
    // for symbols with the same name.
    EXPECT_EQ(&*It++, S2);
    if (&*It == S3) {
      EXPECT_EQ(&*++It, S1);
    } else {
      EXPECT_EQ(&*It++, S1);
      EXPECT_EQ(&*It++, S3);
    }
  }
}

TEST(Unit_Module, findSymbols) {
  auto M = Module::Create(Ctx);
  auto S = Section::Create(Ctx, "test");
  M->addSection(S);
  auto BI = ByteInterval::Create(Ctx, Addr(0), 1);
  M->addByteInterval(S, BI);
  auto* B = CodeBlock::Create(Ctx, 1);
  M->addCodeBlock(BI, B);

  auto* S1 = emplaceSymbol(*M, Ctx, Addr(1), "foo");
  auto* S2 = emplaceSymbol(*M, Ctx, B, "bar");
  auto* S3 = emplaceSymbol(*M, Ctx, Addr(2), "foo");
  auto* S4 = emplaceSymbol(*M, Ctx, B, "baz");

  // Check that symbols are unique even if names and addresses are not.
  M->addSymbol(S3);

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
  auto* M = Module::Create(Ctx);
  emplaceSymbol(*M, Ctx, "test");
  EXPECT_EQ(M->findSymbols("test").begin()->getName(), "test");
}

TEST(Unit_Module, renameSymbol) {
  auto* M = Module::Create(Ctx);
  auto* S1 = emplaceSymbol(*M, Ctx, "foo");
  auto* S2 = emplaceSymbol(*M, Ctx, Addr(1), "bar");
  auto* S3 = emplaceSymbol(*M, Ctx, Addr(2), "bar");

  renameSymbol(*M, *S1, "test1");
  renameSymbol(*M, *S2, "test2");
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
  auto M = Module::Create(Ctx);
  auto S = Section::Create(Ctx, "test");
  M->addSection(S);
  auto BI = ByteInterval::Create(Ctx, Addr(0), 5);
  M->addByteInterval(S, BI);

  auto* B1 = M->emplaceCodeBlock(Ctx, BI, 1, 1);
  auto* B2 = M->emplaceCodeBlock(Ctx, BI, 2, 1);
  auto* B3 = M->emplaceCodeBlock(Ctx, BI, 3, 1);
  auto* B4 = M->emplaceCodeBlock(Ctx, BI, 4, 1);
  auto* B5 = M->emplaceCodeBlock(Ctx, BI, 5, 1);
  auto* S1 = emplaceSymbol(*M, Ctx, "foo");
  auto* S2 = emplaceSymbol(*M, Ctx, B1, "bar");
  auto* S3 = emplaceSymbol(*M, Ctx, B1, "foo");
  auto* S4 = emplaceSymbol(*M, Ctx, B2, "bar");

  setReferent(*M, *S1, B3);
  setReferent(*M, *S2, B4);
  setReferent(*M, *S4, B5);

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
  auto M = Module::Create(Ctx);
  auto S = Section::Create(Ctx, "test");
  M->addSection(S);
  auto BI = ByteInterval::Create(Ctx, Addr(0), 5);
  M->addByteInterval(S, BI);

  auto* B1 = M->emplaceCodeBlock(Ctx, BI, 1, 1);
  auto* S1 = emplaceSymbol(*M, Ctx, "foo");
  auto* S2 = emplaceSymbol(*M, Ctx, B1, "bar");
  auto* S3 = emplaceSymbol(*M, Ctx, B1, "bar");

  setSymbolAddress(*M, *S1, Addr(2));
  setSymbolAddress(*M, *S2, Addr(3));
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
  auto M = Module::Create(Ctx);
  auto S = Section::Create(Ctx, "test");
  M->addSection(S);
  auto BI = ByteInterval::Create(Ctx, Addr(1), 123);
  M->addByteInterval(S, BI);

  Symbol* Sym = Symbol::Create(Ctx);
  M->addSymbolicExpression<SymAddrConst>(BI, 0, 0, Sym);
  EXPECT_EQ(std::distance(M->symbolic_expressions_begin(),
                          M->symbolic_expressions_end()),
            1);
}

TEST(Unit_Module, findSymbolicExpressions) {
  auto M = Module::Create(Ctx);
  auto S = Section::Create(Ctx, "test");
  M->addSection(S);
  auto BI = ByteInterval::Create(Ctx, Addr(0), 10);
  M->addByteInterval(S, BI);

  auto* S1 = Symbol::Create(Ctx, Addr(1), "foo");
  auto* S2 = Symbol::Create(Ctx, Addr(5), "bar");

  M->addSymbolicExpression<SymAddrConst>(BI, 1, 0, S1);
  M->addSymbolicExpression<SymAddrConst>(BI, 5, 0, S2);

  {
    auto F = M->findSymbolicExpression(Addr(1), Addr(5));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(std::get<SymAddrConst>(*F.begin()).Sym, S1);
  }

  {
    auto F = M->findSymbolicExpression(Addr(1), Addr(6));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 2);
    EXPECT_EQ(std::get<SymAddrConst>(*F.begin()).Sym, S1);
    EXPECT_EQ(std::get<SymAddrConst>(*++F.begin()).Sym, S2);
  }

  {
    auto F = M->findSymbolicExpression(Addr(1), Addr(3));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(std::get<SymAddrConst>(*F.begin()).Sym, S1);
  }

  {
    auto F = M->findSymbolicExpression(Addr(6), Addr(50));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 0);
  }
}

// TEST(Unit_Module, getAddrsForSymbolicExpression) {
//   auto* M = Module::Create(Ctx);
//   SymAddrConst SAC1{0, Symbol::Create(Ctx, Addr(1), "foo")};
//   SymAddrConst SAC2{0, Symbol::Create(Ctx, Addr(5), "bar")};
//   SymAddrConst SAC3{0, Symbol::Create(Ctx, Addr(10), "baz")};

//   M->addSymbolicExpression(Addr(1), SAC1);
//   M->addSymbolicExpression(Addr(5), SAC2);
//   M->addSymbolicExpression(Addr(10), SAC1);
//   // Note: SAC3 is purposefully not added to the module while SAC1 is added
//   // twice at different addresses.

//   {
//     auto R = M->getAddrsForSymbolicExpression(SAC1);
//     EXPECT_EQ(std::distance(R.begin(), R.end()), 2);
//     // The order of the results is not guaranteed, so check that both of the
//     // addresses are present but without relying on order.
//     ptrdiff_t Count = std::count_if(R.begin(), R.end(), [](Addr A) {
//       return A == Addr{10} || A == Addr{1};
//     });
//     EXPECT_EQ(Count, 2);
//   }

//   {
//     auto R = M->getAddrsForSymbolicExpression(SAC2);
//     EXPECT_EQ(std::distance(R.begin(), R.end()), 1);
//     EXPECT_EQ(*R.begin(), Addr{5});
//   }

//   {
//     auto R = M->getAddrsForSymbolicExpression(SAC3);
//     EXPECT_EQ(std::distance(R.begin(), R.end()), 0);
//   }
// }

TEST(Unit_Module, protobufRoundTrip) {
  proto::Module Message;

  UUID BlockID, DataID, ProxyID, SectionID;
  size_t WhichSymbolic;

  {
    Context InnerCtx;
    Module* Original = Module::Create(InnerCtx, "module");
    Original->setBinaryPath("test");
    Original->setPreferredAddr(Addr(3));
    Original->setRebaseDelta(4);
    Original->setFileFormat(FileFormat::ELF);
    Original->setISAID(ISAID::X64);
    Original->addAuxData("test", AuxData());
    Original->addSymbol(Symbol::Create(InnerCtx, Addr(1), "name1"));
    Original->addSymbol(Symbol::Create(InnerCtx, Addr(2), "name1"));
    Original->addSymbol(Symbol::Create(InnerCtx, Addr(1), "name3"));
    auto S = Section::Create(InnerCtx);
    Original->addSection(S);
    auto BI = ByteInterval::Create(InnerCtx, Addr(1), 2);
    Original->addByteInterval(S, BI);
    Original->emplaceCodeBlock(InnerCtx, BI, 0, 2);
    Original->addDataBlock(BI, 0, DataBlock::Create(InnerCtx));
    auto* P = ProxyBlock::Create(InnerCtx);
    Original->addProxyBlock(P);
    Original->addSymbolicExpression<SymAddrConst>(BI, 7);
    BlockID = blocks(Original->getCFG()).begin()->getUUID();
    DataID = Original->data_blocks_begin()->getUUID();
    ProxyID = P->getUUID();
    SectionID = Original->section_begin()->getUUID();
    WhichSymbolic = Original->symbolic_expressions_begin()->index();

    Original->toProtobuf(&Message);
  }

  Module* Result = Module::fromProtobuf(Ctx, Message);

  EXPECT_EQ(Result->getBinaryPath(), "test");
  EXPECT_EQ(Result->getPreferredAddr(), Addr(3));
  EXPECT_EQ(Result->getRebaseDelta(), 4);
  EXPECT_EQ(Result->getFileFormat(), FileFormat::ELF);
  EXPECT_EQ(Result->getISAID(), ISAID::X64);
  EXPECT_EQ(Result->getName(), "module");

  // Make sure all symbols are present despite repeated names and addresses.
  EXPECT_EQ(std::distance(Result->symbol_begin(), Result->symbol_end()), 3);
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
  EXPECT_NE(Result->getAuxData("test"), nullptr);

  EXPECT_EQ(num_vertices(Result->getCFG()), 2);
  {
    auto Nodes = nodes(Result->getCFG());
    auto It = Nodes.begin();
    EXPECT_TRUE(&*It);
    EXPECT_TRUE(It->getUUID() == BlockID || It->getUUID() == ProxyID);
    ++It;
    EXPECT_TRUE(&*It);
    EXPECT_NE(Nodes.begin()->getUUID(), It->getUUID());
    EXPECT_TRUE(It->getUUID() == BlockID || It->getUUID() == ProxyID);
  }

  EXPECT_EQ(
      std::distance(Result->data_blocks_begin(), Result->data_blocks_end()), 1);
  EXPECT_EQ(Result->data_blocks_begin()->getUUID(), DataID);

  EXPECT_EQ(std::distance(Result->section_begin(), Result->section_end()), 1);
  EXPECT_EQ(Result->section_begin()->getUUID(), SectionID);

  EXPECT_EQ(std::distance(Result->symbolic_expressions_begin(),
                          Result->symbolic_expressions_end()),
            1);
  EXPECT_EQ(Result->symbolic_expressions_begin()->index(), WhichSymbolic);
}

TEST(Unit_Module, protobufNodePointers) {
  // Ensure that deserialization handles node pointers (e.g. in Symbol and
  // SymbolicExpression) correctly.
  // This is order-dependent: the pointers are serialized as UUIDs, and
  // Node::getByUUID will fail if the corresponding Node has not yet been
  // deserialized.

  proto::Module Message;

  {
    Context InnerCtx;
    Module* Original = Module::Create(InnerCtx);
    auto S = Section::Create(InnerCtx);
    Original->addSection(S);
    auto BI = ByteInterval::Create(InnerCtx, Addr(1), 2);
    Original->addByteInterval(S, BI);
    auto* Data = DataBlock::Create(InnerCtx);
    Original->addDataBlock(BI, 0, Data);
    auto* DataSym = emplaceSymbol(*Original, InnerCtx, Data, "data");

    // Not part of IR
    auto* DanglingData = DataBlock::Create(InnerCtx);
    Original->addSymbol(Symbol::Create(InnerCtx, DanglingData, "dangling"));

    auto* Code = Original->emplaceCodeBlock(InnerCtx, BI, 0, 2);
    emplaceSymbol(*Original, InnerCtx, Code, "code");
    Original->addSymbolicExpression<SymAddrConst>(BI, 2, 0, DataSym);

    // Not part of IR
    auto* DanglingSym = Symbol::Create(InnerCtx, Addr(1), "foo");
    Original->addSymbolicExpression<SymAddrConst>(BI, 3, 0, DanglingSym);

    Original->toProtobuf(&Message);
  }

  Module* Result = Module::fromProtobuf(Ctx, Message);
  EXPECT_NE(Result->findSymbols("data").begin()->getReferent<DataBlock>(),
            nullptr);
  EXPECT_NE(Result->findSymbols("code").begin()->getReferent<CodeBlock>(),
            nullptr);
  // Dangling reference becomes nullptr
  EXPECT_EQ(Result->findSymbols("dangling").begin()->getReferent<CodeBlock>(),
            nullptr);

  const auto& Symbolic =
      get<SymAddrConst>(*Result->findSymbolicExpression(Addr(3)));
  EXPECT_NE(Symbolic.Sym, nullptr);
  EXPECT_EQ(Symbolic.Sym->getName(), "data");

  // Dangling reference becomes nullptr
  EXPECT_EQ(get<SymAddrConst>(*Result->findSymbolicExpression(Addr(4))).Sym,
            nullptr);
}
