#include <gtirb/Block.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/DataObject.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <proto/Module.pb.h>
#include <gtest/gtest.h>
#include <algorithm>
#include <iterator>
#include <tuple>
#include <utility>

using namespace gtirb;

static Context Ctx;

TEST(Unit_Module, ctor_0) { EXPECT_NO_THROW(Module::Create(Ctx)); }

TEST(Unit_Module, setBinaryPath) {
  const std::string StrPath("/home/gt/irb/foo");
  auto *M = Module::Create(Ctx);

  EXPECT_NO_THROW(M->setBinaryPath(StrPath));

  auto Path = M->getBinaryPath();
  EXPECT_EQ(StrPath, Path);
}

TEST(Unit_Module, getFileFormatDefault) {
  auto *M = Module::Create(Ctx);
  EXPECT_EQ(gtirb::FileFormat::Undefined, M->getFileFormat());
}

TEST(Unit_Module, setFileFormat) {
  auto *M = Module::Create(Ctx);

  EXPECT_NO_THROW(M->setFileFormat(gtirb::FileFormat::COFF));
  EXPECT_EQ(gtirb::FileFormat::COFF, M->getFileFormat());

  EXPECT_NO_THROW(M->setFileFormat(gtirb::FileFormat::MACHO));
  EXPECT_EQ(gtirb::FileFormat::MACHO, M->getFileFormat());

  EXPECT_NO_THROW(M->setFileFormat(gtirb::FileFormat::Undefined));
  EXPECT_EQ(gtirb::FileFormat::Undefined, M->getFileFormat());
}

TEST(Unit_Module, getRebaseDeltaDefault) {
  auto *M = Module::Create(Ctx);
  EXPECT_EQ(int64_t{0}, M->getRebaseDelta());
}

TEST(Unit_Module, setRebaseDelta) {
  auto *M = Module::Create(Ctx);

  EXPECT_NO_THROW(M->setRebaseDelta(1));
  EXPECT_EQ(int64_t{1}, M->getRebaseDelta());

  EXPECT_NO_THROW(M->setRebaseDelta(-1));
  EXPECT_EQ(int64_t{-1}, M->getRebaseDelta());

  EXPECT_NO_THROW(M->setRebaseDelta(std::numeric_limits<int64_t>::max()));
  EXPECT_EQ(std::numeric_limits<int64_t>::max(), M->getRebaseDelta());

  EXPECT_NO_THROW(M->setRebaseDelta(std::numeric_limits<int64_t>::min()));
  EXPECT_EQ(std::numeric_limits<int64_t>::min(), M->getRebaseDelta());

  EXPECT_NO_THROW(M->setRebaseDelta(std::numeric_limits<int64_t>::lowest()));
  EXPECT_EQ(std::numeric_limits<int64_t>::lowest(), M->getRebaseDelta());
}

TEST(Unit_Module, getPreferredAddrDefault) {
  auto *M = Module::Create(Ctx);

  EXPECT_NO_THROW(M->getPreferredAddr());
  EXPECT_EQ(Addr{}, M->getPreferredAddr());
}

TEST(Unit_Module, getISAID) {
  auto *M = Module::Create(Ctx);

  EXPECT_NO_THROW(M->getISAID());
  EXPECT_EQ(gtirb::ISAID::Undefined, M->getISAID());

  EXPECT_NO_THROW(M->setISAID(gtirb::ISAID::X64));
  EXPECT_EQ(gtirb::ISAID::X64, M->getISAID());
}

TEST(Unit_Module, setPreferredAddr) {
  auto *M = Module::Create(Ctx);
  Addr Preferred{64};

  EXPECT_NO_THROW(M->getPreferredAddr());
  EXPECT_NO_THROW(M->setPreferredAddr(Preferred));

  EXPECT_EQ(Preferred, M->getPreferredAddr());
}

TEST(Unit_Module, getSymbolSet) {
  auto *M = Module::Create(Ctx);
  EXPECT_NO_THROW(M->symbols());
}

TEST(Unit_Module, getImageByteMap) {
  auto *M = Module::Create(Ctx);
  EXPECT_NO_THROW(M->getImageByteMap());
}

TEST(Unit_Module, setName) {
  const std::string Name{"foo"};
  auto *M = Module::Create(Ctx);

  EXPECT_NO_THROW(M->setName(Name));
  EXPECT_EQ(Name, M->getName());
}

TEST(Unit_Module, getName) {
  auto *M = Module::Create(Ctx);
  EXPECT_NO_THROW(M->getName());
  EXPECT_TRUE(M->getName().empty());
}

TEST(Unit_Module, sections) {
  auto *M = Module::Create(Ctx);
  M->addSection(Section::Create(Ctx, "test", Addr(), 123));
  EXPECT_EQ(M->section_begin()->getName(), "test");
}

TEST(Unit_Module, dataObjects) {
  auto *M = Module::Create(Ctx);
  M->addData(DataObject::Create(Ctx, Addr(1), 123));
  EXPECT_EQ(M->data_begin()->getAddress(), Addr(1));
}

TEST(Unit_Module, symbolicExpressions) {
  auto *M = Module::Create(Ctx);
  Symbol *S = Symbol::Create(Ctx);
  M->addSymbolicExpression(Addr(1), SymAddrConst{0, S});
  EXPECT_EQ(std::distance(M->symbolic_expr_begin(), M->symbolic_expr_end()), 1);
}

TEST(Unit_Module, protobufRoundTrip) {
  proto::Module Message;

  UUID ByteMapID, SymbolID, BlockID, DataID, SectionID;
  size_t WhichSymbolic;

  {
    Context InnerCtx;
    Module *Original = Module::Create(InnerCtx);
    Original->setBinaryPath("test");
    Original->setPreferredAddr(Addr(3));
    Original->setRebaseDelta(4);
    Original->setFileFormat(FileFormat::ELF);
    Original->setISAID(ISAID::X64);
    Original->setName("module");
    Original->addSymbol(Symbol::Create(InnerCtx));
    addBlock(Original->getCFG(), Block::Create(InnerCtx));
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

  Module *Result = Module::fromProtobuf(Ctx, Message);

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

TEST(Unit_Module, sectionSorting) {
  Section* S1 = Section::Create(Ctx, "second", Addr(0), 1);
  Section* S2 = Section::Create(Ctx, "first", Addr(0), 1);
  Module* M = Module::Create(Ctx);

  M->addSection({S1, S2});

  UUID U1 = S1->getUUID(), U2 = S2->getUUID();
  EXPECT_EQ(M->section_begin()->getUUID(), U1);
  EXPECT_EQ((M->section_begin() + 1)->getUUID(), U2);

  std::sort(M->section_begin(), M->section_end(),
            [](const Section& LHS, const Section& RHS) {
              return LHS.getName() < RHS.getName();
            });

  EXPECT_EQ(M->section_begin()->getUUID(), U2);
  EXPECT_EQ((M->section_begin() + 1)->getUUID(), U1);
}
