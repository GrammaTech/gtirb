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
#include <gtirb/Block.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/DataObject.hpp>
#include <gtirb/ImageByteMap.hpp>
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
  static_assert(std::is_same_v<Module::block_iterator::reference, Block&>);
  static_assert(
      std::is_same_v<Module::const_block_iterator::reference, const Block&>);
  static_assert(
      std::is_same_v<Module::block_subrange::iterator::reference, Block&>);
  static_assert(
      std::is_same_v<Module::const_block_subrange::iterator::reference,
                     const Block&>);
  // Actually calling the constructor and assignment operator tends to produce
  // more informative error messages than std::is_constructible and
  // std::is_assignable.
  {
    Module::block_iterator it;
    Module::const_block_iterator cit(it);
    cit = it;
  }

  {
    Module::block_subrange::iterator it;
    Module::const_block_subrange::iterator cit(it);
    cit = it;
  }

  static_assert(
      std::is_same_v<Module::data_object_iterator::reference, DataObject&>);
  static_assert(std::is_same_v<Module::const_data_object_iterator::reference,
                               const DataObject&>);
  static_assert(
      std::is_same_v<Module::data_object_subrange::iterator::reference,
                     DataObject&>);
  static_assert(
      std::is_same_v<Module::const_data_object_subrange::iterator::reference,
                     const DataObject&>);
  {
    Module::data_object_iterator it;
    Module::const_data_object_iterator cit(it);
    cit = it;
  }

  {
    Module::data_object_subrange::iterator it;
    Module::const_data_object_subrange::iterator cit(it);
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

  {
    Module::section_subrange::iterator it;
    Module::const_section_subrange::iterator cit(it);
    cit = it;
  }

  // There are no non-const symbolic_expr_iterators...
  static_assert(std::is_same_v<Module::const_symbolic_expr_iterator::reference,
                               const SymbolicExpression&>);
  static_assert(
      std::is_same_v<Module::const_symbolic_expr_addr_iterator::reference,
                     const Addr&>);

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

  {
    Module::symbol_addr_iterator it;
    Module::const_symbol_addr_iterator cit(it);
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
  M->addSection(Section::Create(Ctx, "test", Addr(), 123));
  EXPECT_EQ(M->section_begin()->getName(), "test");
  EXPECT_EQ(std::distance(M->section_begin(), M->section_end()), 1);
  EXPECT_EQ(std::distance(M->section_by_name_begin(), M->section_by_name_end()),
            1);
}

TEST(Unit_Module, sectionIterationOrder) {
  auto* M = Module::Create(Ctx);
  auto* S = Section::Create(Ctx, "", Addr(0), 10);
  M->addSection(S);
  M->addSection(Section::Create(Ctx, "", Addr(0), 5));
  M->addSection(Section::Create(Ctx, "", Addr(5), 5));
  M->addSection(Section::Create(Ctx, "", Addr(5), 5)); // new object is added
  M->addSection(S);                                    // ignored

  EXPECT_EQ(std::distance(M->section_begin(), M->section_end()), 4);
  auto It = M->section_begin();
  EXPECT_EQ(It->getAddress(), Addr(0));
  EXPECT_EQ(It->getSize(), 5);
  ++It;
  EXPECT_EQ(It->getAddress(), Addr(0));
  EXPECT_EQ(It->getSize(), 10);
  ++It;
  EXPECT_EQ(It->getAddress(), Addr(5));
  EXPECT_EQ(It->getSize(), 5);
  ++It;
  EXPECT_EQ(It->getAddress(), Addr(5));
  EXPECT_EQ(It->getSize(), 5);
}

TEST(Unit_Module, findSection) {
  auto* M = Module::Create(Ctx);
  M->addSection(Section::Create(Ctx, "test", Addr(1), 123));

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
  auto* B = emplaceBlock(*M, Ctx, Addr(1), 10);
  // Second add should have no effect
  M->addBlock(B);

  EXPECT_EQ(std::distance(M->block_begin(), M->block_end()), 1);
  EXPECT_EQ(M->block_begin()->getAddress(), Addr(1));

  auto F = blocks(M->getCFG());
  EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
  EXPECT_EQ(F.begin()->getAddress(), Addr(1));
}

TEST(Unit_Module, cfgNodes) {
  auto* M = Module::Create(Ctx);
  auto* B = Block::Create(Ctx, Addr(1), 10);
  auto* P = ProxyBlock::Create(Ctx);
  M->addCfgNode(B);
  M->addCfgNode(P);

  EXPECT_EQ(std::distance(M->block_begin(), M->block_end()), 1);
  auto Nodes = nodes(M->getCFG());
  EXPECT_EQ(std::distance(Nodes.begin(), Nodes.end()), 2);
  auto It = Nodes.begin();
  EXPECT_TRUE(&*It == B || &*It == P);
  ++It;
  EXPECT_NE(&*Nodes.begin(), &*It);
  EXPECT_TRUE(&*It == B || &*It == P);
}

TEST(Unit_Module, blockIterationOrder) {
  auto* M = Module::Create(Ctx);
  auto* B = emplaceBlock(*M, Ctx, Addr(0), 10);
  emplaceBlock(*M, Ctx, Addr(0), 5);
  emplaceBlock(*M, Ctx, Addr(5), 5);
  emplaceBlock(*M, Ctx, Addr(5), 5); // new object is added
  M->addBlock(B);                    // ignored

  EXPECT_EQ(std::distance(M->block_begin(), M->block_end()), 4);
  auto It = M->block_begin();
  EXPECT_EQ(It->getAddress(), Addr(0));
  EXPECT_EQ(It->getSize(), 5);
  ++It;
  EXPECT_EQ(It->getAddress(), Addr(0));
  EXPECT_EQ(It->getSize(), 10);
  ++It;
  EXPECT_EQ(It->getAddress(), Addr(5));
  EXPECT_EQ(It->getSize(), 5);
  ++It;
  EXPECT_EQ(It->getAddress(), Addr(5));
  EXPECT_EQ(It->getSize(), 5);
}

TEST(Unit_Module, findBlock) {
  auto M = Module::Create(Ctx);
  auto* B1 = emplaceBlock(*M, Ctx, Addr(1), 20);
  auto* B2 = emplaceBlock(*M, Ctx, Addr(5), 10);

  {
    auto F = M->findBlock(Addr(0));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 0);

    F = M->findBlock(Addr(1));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), B1);

    F = M->findBlock(Addr(5));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 2);
    EXPECT_EQ(&*F.begin(), B1);
    EXPECT_EQ(&*++F.begin(), B2);

    F = M->findBlock(Addr(14));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 2);
    EXPECT_EQ(&*F.begin(), B1);
    EXPECT_EQ(&*++F.begin(), B2);

    F = M->findBlock(Addr(15));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), B1);

    F = M->findBlock(Addr(20));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), B1);

    F = M->findBlock(Addr(21));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 0);
  }
}

TEST(Unit_Module, dataObjects) {
  auto* M = Module::Create(Ctx);
  M->addData(DataObject::Create(Ctx, Addr(1), 123));
  EXPECT_EQ(M->data_begin()->getAddress(), Addr(1));
}

TEST(Unit_Module, dataObjectIterationOrder) {
  auto* M = Module::Create(Ctx);
  auto* D = DataObject::Create(Ctx, Addr(0), 10);
  M->addData(D);
  M->addData(DataObject::Create(Ctx, Addr(0), 5));
  M->addData(DataObject::Create(Ctx, Addr(5), 5));
  M->addData(DataObject::Create(Ctx, Addr(5), 5)); // new object is added
  M->addData(D);                                   // ignored

  EXPECT_EQ(std::distance(M->data_begin(), M->data_end()), 4);
  auto It = M->data_begin();
  EXPECT_EQ(It->getAddress(), Addr(0));
  EXPECT_EQ(It->getSize(), 5);
  ++It;
  EXPECT_EQ(It->getAddress(), Addr(0));
  EXPECT_EQ(It->getSize(), 10);
  ++It;
  EXPECT_EQ(It->getAddress(), Addr(5));
  EXPECT_EQ(It->getSize(), 5);
  ++It;
  EXPECT_EQ(It->getAddress(), Addr(5));
  EXPECT_EQ(It->getSize(), 5);
}

TEST(Unit_Module, findData) {
  auto* M = Module::Create(Ctx);
  auto* D1 = DataObject::Create(Ctx, Addr(1), 20);
  auto* D2 = DataObject::Create(Ctx, Addr(5), 10);
  M->addData(D1);
  M->addData(D2);

  {
    auto F = M->findData(Addr(0));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 0);

    F = M->findData(Addr(1));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), D1);

    F = M->findData(Addr(5));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 2);
    EXPECT_EQ(&*F.begin(), D1);
    EXPECT_EQ(&*(++F.begin()), D2);

    F = M->findData(Addr(14));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 2);
    EXPECT_EQ(&*F.begin(), D1);
    EXPECT_EQ(&*(++F.begin()), D2);

    F = M->findData(Addr(15));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), D1);

    F = M->findData(Addr(20));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), D1);

    F = M->findData(Addr(21));
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
  auto* M = Module::Create(Ctx);
  auto* S1 = emplaceSymbol(*M, Ctx, Addr(1), "foo");
  auto* S2 = emplaceSymbol(*M, Ctx, Addr(1), "bar");
  auto* S3 = emplaceSymbol(*M, Ctx, Addr(2), "foo");

  // Check that symbols are unique even if names and addresses are not.
  M->addSymbol(S3);

  {
    auto F = M->findSymbols("foo");
    EXPECT_EQ(std::distance(F.begin(), F.end()), 2);
    EXPECT_EQ(&*F.begin(), S1);
    EXPECT_EQ(&*(++F.begin()), S3);
  }

  {
    auto F = M->findSymbols("bar");
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), S2);
  }

  EXPECT_TRUE(M->findSymbols("notfound").empty());

  {
    auto F = M->findSymbols(Addr(1));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 2);
    EXPECT_EQ(&*F.begin(), S1);
    EXPECT_EQ(&*(++F.begin()), S2);
  }

  {
    auto F = M->findSymbols(Addr(2));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 1);
    EXPECT_EQ(&*F.begin(), S3);
  }

  EXPECT_TRUE(M->findSymbols(Addr(3)).empty());

  {
    auto F = M->findSymbols(Addr(0), Addr(2));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 2);
    EXPECT_EQ(&*F.begin(), S1);
    EXPECT_EQ(&*std::next(F.begin(), 1), S2);
  }

  {
    auto F = M->findSymbols(Addr(0), Addr(5));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 3);
    EXPECT_EQ(&*F.begin(), S1);
    EXPECT_EQ(&*std::next(F.begin(), 1), S2);
    EXPECT_EQ(&*std::next(F.begin(), 2), S3);
  }

  {
    auto F = M->findSymbols(Addr(10), Addr(25));
    EXPECT_EQ(std::distance(F.begin(), F.end()), 0);
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
  auto* M = Module::Create(Ctx);
  auto* B1 = emplaceBlock(*M, Ctx, Addr(1), 1);
  auto* B2 = emplaceBlock(*M, Ctx, Addr(2), 1);
  auto* B3 = emplaceBlock(*M, Ctx, Addr(3), 1);
  auto* B4 = emplaceBlock(*M, Ctx, Addr(4), 1);
  auto* B5 = emplaceBlock(*M, Ctx, Addr(5), 1);
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
  auto* M = Module::Create(Ctx);
  auto* B1 = emplaceBlock(*M, Ctx, Addr(1), 1);
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
  auto* M = Module::Create(Ctx);
  Symbol* S = Symbol::Create(Ctx);
  M->addSymbolicExpression(Addr(1), SymAddrConst{0, S});
  EXPECT_EQ(std::distance(M->symbolic_expr_begin(), M->symbolic_expr_end()), 1);
}

TEST(Unit_Module, symbolicExpressionIterationOrder) {
  auto* M = Module::Create(Ctx);
  Symbol* S1 = Symbol::Create(Ctx);
  Symbol* S2 = Symbol::Create(Ctx);
  M->addSymbolicExpression(Addr(2), SymAddrAddr{0, 1, S1, S2});
  M->addSymbolicExpression(Addr(1), SymAddrConst{0, S1});
  M->addSymbolicExpression(Addr(3), SymAddrConst{0, S1});
  // Note: This should replace the previous expression at Addr(1).
  M->addSymbolicExpression(Addr(1), SymStackConst{0, S2});

  {
    // symbolic_expr_iterator returns in address order
    auto Rng = M->symbolic_exprs();
    EXPECT_EQ(std::distance(Rng.begin(), Rng.end()), 3);
    auto It = M->symbolic_expr_begin();
    EXPECT_TRUE(std::holds_alternative<SymStackConst>(*It));
    EXPECT_EQ(std::get<SymStackConst>(*It).Sym, S2);
    ++It;
    EXPECT_TRUE(std::holds_alternative<SymAddrAddr>(*It));
    EXPECT_EQ(std::get<SymAddrAddr>(*It).Sym1, S1);
    EXPECT_EQ(std::get<SymAddrAddr>(*It).Sym2, S2);
    ++It;
    EXPECT_TRUE(std::holds_alternative<SymAddrConst>(*It));
    EXPECT_EQ(std::get<SymAddrConst>(*It).Sym, S1);
  }
}

TEST(Unit_Module, findSymbolicExpressions) {
  auto* M = Module::Create(Ctx);
  auto* S1 = Symbol::Create(Ctx, Addr(1), "foo");
  auto* S2 = Symbol::Create(Ctx, Addr(5), "bar");

  M->addSymbolicExpression(Addr(1), SymAddrConst{0, S1});
  M->addSymbolicExpression(Addr(5), SymAddrConst{0, S2});

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

TEST(Unit_Module, getAddrsForSymbolicExpression) {
  auto* M = Module::Create(Ctx);
  SymAddrConst SAC1{0, Symbol::Create(Ctx, Addr(1), "foo")};
  SymAddrConst SAC2{0, Symbol::Create(Ctx, Addr(5), "bar")};
  SymAddrConst SAC3{0, Symbol::Create(Ctx, Addr(10), "baz")};

  M->addSymbolicExpression(Addr(1), SAC1);
  M->addSymbolicExpression(Addr(5), SAC2);
  M->addSymbolicExpression(Addr(10), SAC1);
  // Note: SAC3 is purposefully not added to the module while SAC1 is added
  // twice at different addresses.

  {
    auto R = M->getAddrsForSymbolicExpression(SAC1);
    EXPECT_EQ(std::distance(R.begin(), R.end()), 2);
    // The order of the results is not guaranteed, so check that both of the
    // addresses are present but without relying on order.
    ptrdiff_t Count = std::count_if(R.begin(), R.end(), [](Addr A) {
      return A == Addr{10} || A == Addr{1};
    });
    EXPECT_EQ(Count, 2);
  }

  {
    auto R = M->getAddrsForSymbolicExpression(SAC2);
    EXPECT_EQ(std::distance(R.begin(), R.end()), 1);
    EXPECT_EQ(*R.begin(), Addr{5});
  }

  {
    auto R = M->getAddrsForSymbolicExpression(SAC3);
    EXPECT_EQ(std::distance(R.begin(), R.end()), 0);
  }
}

TEST(Unit_Module, protobufRoundTrip) {
  proto::Module Message;

  UUID ByteMapID, BlockID, DataID, ProxyID, SectionID;
  size_t WhichSymbolic;

  {
    Context InnerCtx;
    Module* Original = Module::Create(InnerCtx, "module");
    Original->setBinaryPath("test");
    Original->setPreferredAddr(Addr(3));
    Original->setRebaseDelta(4);
    Original->setFileFormat(FileFormat::ELF);
    Original->setISAID(ISAID::X64);
    Original->addSymbol(Symbol::Create(InnerCtx, Addr(1), "name1"));
    Original->addSymbol(Symbol::Create(InnerCtx, Addr(2), "name1"));
    Original->addSymbol(Symbol::Create(InnerCtx, Addr(1), "name3"));
    emplaceBlock(*Original, InnerCtx, Addr(1), 2);
    Original->addData(DataObject::Create(InnerCtx));
    auto* P = ProxyBlock::Create(InnerCtx);
    Original->addProxyBlock(P);
    Original->addSection(Section::Create(InnerCtx));
    Original->addSymbolicExpression(Addr(7), {SymAddrConst()});
    ByteMapID = Original->getImageByteMap().getUUID();
    BlockID = blocks(Original->getCFG()).begin()->getUUID();
    DataID = Original->data_begin()->getUUID();
    ProxyID = P->getUUID();
    SectionID = Original->section_begin()->getUUID();
    WhichSymbolic = Original->symbolic_expr_begin()->index();

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
  EXPECT_EQ(Result->getImageByteMap().getUUID(), ByteMapID);

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

  EXPECT_EQ(std::distance(Result->data_begin(), Result->data_end()), 1);
  EXPECT_EQ(Result->data_begin()->getUUID(), DataID);

  EXPECT_EQ(std::distance(Result->section_begin(), Result->section_end()), 1);
  EXPECT_EQ(Result->section_begin()->getUUID(), SectionID);

  EXPECT_EQ(
      std::distance(Result->symbolic_expr_begin(), Result->symbolic_expr_end()),
      1);
  EXPECT_EQ(Result->symbolic_expr_begin()->index(), WhichSymbolic);
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
    auto* Data = DataObject::Create(InnerCtx);
    Original->addData(Data);
    auto* DataSym = emplaceSymbol(*Original, InnerCtx, Data, "data");

    // Not part of IR
    auto* DanglingData = DataObject::Create(InnerCtx);
    Original->addSymbol(Symbol::Create(InnerCtx, DanglingData, "dangling"));

    auto* Code = emplaceBlock(*Original, InnerCtx, Addr(1), 2);
    emplaceSymbol(*Original, InnerCtx, Code, "code");
    Original->addSymbolicExpression(Addr(3), {SymAddrConst{0, DataSym}});

    // Not part of IR
    auto* DanglingSym = Symbol::Create(InnerCtx, Addr(1), "foo");
    Original->addSymbolicExpression(Addr(4), {SymAddrConst{0, DanglingSym}});

    Original->toProtobuf(&Message);
  }

  Module* Result = Module::fromProtobuf(Ctx, Message);
  EXPECT_NE(Result->findSymbols("data").begin()->getReferent<DataObject>(),
            nullptr);
  EXPECT_NE(Result->findSymbols("code").begin()->getReferent<Block>(), nullptr);
  // Dangling reference becomes nullptr
  EXPECT_EQ(Result->findSymbols("dangling").begin()->getReferent<Block>(),
            nullptr);

  const auto& Symbolic =
      get<SymAddrConst>(*Result->findSymbolicExpression(Addr(3)));
  EXPECT_NE(Symbolic.Sym, nullptr);
  EXPECT_EQ(Symbolic.Sym->getName(), "data");

  // Dangling reference becomes nullptr
  EXPECT_EQ(get<SymAddrConst>(*Result->findSymbolicExpression(Addr(4))).Sym,
            nullptr);
}
