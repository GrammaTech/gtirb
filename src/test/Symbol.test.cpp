#include <gtirb/Block.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/DataObject.hpp>
#include <gtirb/Symbol.hpp>
#include <proto/Symbol.pb.h>
#include <gtest/gtest.h>
#include <memory>

using namespace gtirb;

static Context Ctx;

TEST(Unit_Symbol, ctor_0) { EXPECT_NO_THROW(Symbol::Create(Ctx)); }

TEST(Unit_Symbol, setName) {
  const std::string Value{"Foo"};

  auto *Node = Symbol::Create(Ctx);
  EXPECT_NO_THROW(Node->getName());
  EXPECT_TRUE(Node->getName().empty());

  EXPECT_NO_THROW(Node->setName(Value));
  EXPECT_EQ(Value, Node->getName());
}

TEST(Unit_Symbol, setEA) {
  const gtirb::EA Value{22678};

  auto *Node = Symbol::Create(Ctx);
  EXPECT_NO_THROW(Node->getEA());
  EXPECT_EQ(gtirb::EA{}, Node->getEA());

  EXPECT_NO_THROW(Node->setEA(Value));
  EXPECT_EQ(Value, Node->getEA());
}

TEST(Unit_Symbol, setStorageKind) {
  const gtirb::Symbol::StorageKind Value{gtirb::Symbol::StorageKind::Static};

  auto *Node = Symbol::Create(Ctx);
  EXPECT_NO_THROW(Node->getStorageKind());
  EXPECT_EQ(gtirb::Symbol::StorageKind::Extern, Node->getStorageKind());

  EXPECT_NO_THROW(Node->setStorageKind(Value));
  EXPECT_EQ(Value, Node->getStorageKind());
}

TEST(Unit_Symbol, setReferent) {
  Symbol *Sym = Symbol::Create(Ctx);
  DataObject *Data = DataObject::Create(Ctx);
  Block *Block = Block::Create(Ctx);

  Sym->setReferent(*Data);
  EXPECT_EQ(&*Sym->getDataReferent(), Data);
  EXPECT_FALSE(Sym->getCodeReferent());

  Sym->setReferent(*Block);
  EXPECT_EQ(&*Sym->getCodeReferent(), Block);
  // Setting code referent clears data referent
  EXPECT_FALSE(Sym->getDataReferent());

  Sym->setReferent(*Data);
  EXPECT_EQ(&*Sym->getDataReferent(), Data);
  // Setting data referent clears code referent
  EXPECT_FALSE(Sym->getCodeReferent());
}

TEST(Unit_Symbol, protobufRoundTrip) {
  proto::Symbol Message;
  UUID DataUUID;

  {
    Symbol *Original = Symbol::Create(Ctx, EA(1), "test");
    Original->setStorageKind(Symbol::StorageKind::Static);

    DataObject *Data = DataObject::Create(Ctx);
    DataUUID = Data->getUUID();
    Original->setReferent(*Data);

    Original->toProtobuf(&Message);

    details::ClearUUIDs(Data);
    details::ClearUUIDs(Original);
  }

  Symbol *Result = Symbol::fromProtobuf(Ctx, Message);

  EXPECT_EQ(Result->getEA(), EA(1));
  EXPECT_EQ(Result->getName(), "test");
  EXPECT_EQ(Result->getStorageKind(), Symbol::StorageKind::Static);
  EXPECT_EQ(Result->getDataReferent().getUUID(), DataUUID);
  EXPECT_EQ(Result->getCodeReferent().getUUID(), UUID());
}
