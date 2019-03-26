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

TEST(Unit_Module, setName) {
  const std::string Name{"foo"};
  auto* M = Module::Create(Ctx);

  M->setName(Name);
  EXPECT_EQ(Name, M->getName());
}

TEST(Unit_Module, getName) {
  auto* M = Module::Create(Ctx);
  EXPECT_TRUE(M->getName().empty());
}

TEST(Unit_Module, sections) {
  auto* M = Module::Create(Ctx);
  M->addSection(Section::Create(Ctx, "test", Addr(), 123));
  EXPECT_EQ(M->section_begin()->getName(), "test");
}

TEST(Unit_Module, findSection) {
  auto* M = Module::Create(Ctx);
  M->addSection(Section::Create(Ctx, "test", Addr(1), 123));
  EXPECT_EQ(M->findSection(Addr(1))->getName(), "test");
  EXPECT_EQ(M->findSection(Addr(2)), M->section_end());
}

TEST(Unit_Module, dataObjects) {
  auto* M = Module::Create(Ctx);
  M->addData(DataObject::Create(Ctx, Addr(1), 123));
  EXPECT_EQ(M->data_begin()->getAddress(), Addr(1));
}

TEST(Unit_Module, findData) {
  auto* M = Module::Create(Ctx);
  auto* D1 = DataObject::Create(Ctx, Addr(1), 20);
  auto* D2 = DataObject::Create(Ctx, Addr(5), 10);
  M->addData(D1);
  M->addData(D2);

  {
    auto F = M->findData(Addr(1));
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
  auto* B1 = emplaceBlock(M->getCFG(), Ctx, Addr(1), 1);
  auto* B2 = emplaceBlock(M->getCFG(), Ctx, Addr(2), 1);
  auto* B3 = emplaceBlock(M->getCFG(), Ctx, Addr(3), 1);
  auto* B4 = emplaceBlock(M->getCFG(), Ctx, Addr(4), 1);
  auto* B5 = emplaceBlock(M->getCFG(), Ctx, Addr(5), 1);
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
  auto* B1 = emplaceBlock(M->getCFG(), Ctx, Addr(1), 1);
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

  UUID ByteMapID, BlockID, DataID, SectionID;
  size_t WhichSymbolic;

  {
    Context InnerCtx;
    Module* Original = Module::Create(InnerCtx);
    Original->setBinaryPath("test");
    Original->setPreferredAddr(Addr(3));
    Original->setRebaseDelta(4);
    Original->setFileFormat(FileFormat::ELF);
    Original->setISAID(ISAID::X64);
    Original->setName("module");
    Original->addSymbol(Symbol::Create(InnerCtx, Addr(1), "name1"));
    Original->addSymbol(Symbol::Create(InnerCtx, Addr(2), "name1"));
    Original->addSymbol(Symbol::Create(InnerCtx, Addr(1), "name3"));
    emplaceBlock(Original->getCFG(), InnerCtx, Addr(1), 2);
    Original->addData(DataObject::Create(InnerCtx));
    Original->addSection(Section::Create(InnerCtx));
    Original->addSymbolicExpression(Addr(7), {SymAddrConst()});
    ByteMapID = Original->getImageByteMap().getUUID();
    BlockID = blocks(Original->getCFG()).begin()->getUUID();
    DataID = Original->data_begin()->getUUID();
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

  EXPECT_EQ(num_vertices(Result->getCFG()), 1);
  EXPECT_EQ(blocks(Result->getCFG()).begin()->getUUID(), BlockID);

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

    auto* Code = emplaceBlock(Original->getCFG(), InnerCtx, Addr(1), 2);
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
