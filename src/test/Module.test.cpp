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

  M->setRebaseDelta(1);
  EXPECT_EQ(int64_t{1}, M->getRebaseDelta());

  M->setRebaseDelta(-1);
  EXPECT_EQ(int64_t{-1}, M->getRebaseDelta());

  M->setRebaseDelta(std::numeric_limits<int64_t>::max());
  EXPECT_EQ(std::numeric_limits<int64_t>::max(), M->getRebaseDelta());

  M->setRebaseDelta(std::numeric_limits<int64_t>::min());
  EXPECT_EQ(std::numeric_limits<int64_t>::min(), M->getRebaseDelta());

  M->setRebaseDelta(std::numeric_limits<int64_t>::lowest());
  EXPECT_EQ(std::numeric_limits<int64_t>::lowest(), M->getRebaseDelta());
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
  M->addData(DataObject::Create(Ctx, Addr(1), 123));

  EXPECT_EQ(M->findData(Addr(1))->getAddress(), Addr(1));
  EXPECT_EQ(M->findData(Addr(2)), M->data_end());
}

TEST(Unit_Module, findSymbols) {
  auto* M = Module::Create(Ctx);
  M->addSymbol(Symbol::Create(Ctx, Addr(1), "test"));
  EXPECT_EQ(M->findSymbol("test")->getName(), "test");
  EXPECT_EQ(M->findSymbol("notfound"), M->symbol_end());

  EXPECT_EQ(M->findSymbols(Addr(1)).begin()->getName(), "test");
  EXPECT_TRUE(M->findSymbols(Addr(2)).empty());
}

TEST(Unit_Module, symbolWithoutAddr) {
  auto* M = Module::Create(Ctx);
  M->addSymbol(Symbol::Create(Ctx, "test"));
  EXPECT_EQ(M->findSymbol("test")->getName(), "test");
}

TEST(Unit_Module, symbolicExpressions) {
  auto* M = Module::Create(Ctx);
  Symbol* S = Symbol::Create(Ctx);
  M->addSymbolicExpression(Addr(1), SymAddrConst{0, S});
  EXPECT_EQ(std::distance(M->symbolic_expr_begin(), M->symbolic_expr_end()), 1);
}

TEST(Unit_Module, protobufRoundTrip) {
  proto::Module Message;

  UUID ByteMapID, SymbolID, BlockID, DataID, SectionID;
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
    Original->addSymbol(Symbol::Create(InnerCtx));
    emplaceBlock(Original->getCFG(), InnerCtx, Addr(1), 2);
    Original->addData(DataObject::Create(InnerCtx));
    Original->addSection(Section::Create(InnerCtx));
    Original->addSymbolicExpression(Addr(7), {SymAddrConst()});
    ByteMapID = Original->getImageByteMap().getUUID();
    SymbolID = Original->symbol_begin()->getUUID();
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

  // Make sure various collections and node members are serialized, but
  // don't check in detail as they have their own unit tests.
  EXPECT_EQ(Result->getImageByteMap().getUUID(), ByteMapID);

  EXPECT_EQ(num_vertices(Result->getCFG()), 1);
  EXPECT_EQ(blocks(Result->getCFG()).begin()->getUUID(), BlockID);

  EXPECT_EQ(std::distance(Result->symbol_begin(), Result->symbol_end()), 1);
  EXPECT_EQ(Result->symbol_begin()->getUUID(), SymbolID);

  EXPECT_EQ(std::distance(Result->data_begin(), Result->data_end()), 1);
  EXPECT_EQ(Result->data_begin()->getUUID(), DataID);

  EXPECT_EQ(std::distance(Result->section_begin(), Result->section_end()), 1);
  EXPECT_EQ(Result->section_begin()->getUUID(), SectionID);

  EXPECT_EQ(
      std::distance(Result->symbolic_expr_begin(), Result->symbolic_expr_end()),
      1);
  EXPECT_EQ(Result->symbolic_expr_begin()->index(), WhichSymbolic);
}
