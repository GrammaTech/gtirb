#include <gtirb/Block.hpp>
#include <gtirb/DataObject.hpp>
#include <gtirb/Symbol.hpp>
#include <proto/Symbol.pb.h>
#include <gtest/gtest.h>
#include <memory>

using namespace gtirb;

TEST(Unit_Symbol, ctor_0) { EXPECT_NO_THROW(gtirb::Symbol()); }

TEST(Unit_Symbol, setName) {
  const std::string Value{"Foo"};

  auto Node = std::make_unique<gtirb::Symbol>();
  EXPECT_NO_THROW(Node->getName());
  EXPECT_TRUE(Node->getName().empty());

  EXPECT_NO_THROW(Node->setName(Value));
  EXPECT_EQ(Value, Node->getName());
}

TEST(Unit_Symbol, setAddress) {
  const Addr Value{22678};

  auto Node = std::make_unique<gtirb::Symbol>();
  EXPECT_NO_THROW(Node->getAddress());
  EXPECT_EQ(Addr{}, Node->getAddress());

  EXPECT_NO_THROW(Node->setAddress(Value));
  EXPECT_EQ(Value, Node->getAddress());
}

TEST(Unit_Symbol, setStorageKind) {
  const gtirb::Symbol::StorageKind Value{gtirb::Symbol::StorageKind::Static};

  auto Node = std::make_unique<gtirb::Symbol>();
  EXPECT_NO_THROW(Node->getStorageKind());
  EXPECT_EQ(gtirb::Symbol::StorageKind::Extern, Node->getStorageKind());

  EXPECT_NO_THROW(Node->setStorageKind(Value));
  EXPECT_EQ(Value, Node->getStorageKind());
}

TEST(Unit_Symbol, setReferent) {
  Symbol Sym;
  DataObject Data;
  Block Block;

  Sym.setReferent(Data);
  EXPECT_EQ(&*Sym.getDataReferent(), &Data);
  EXPECT_FALSE(Sym.getCodeReferent());

  Sym.setReferent(Block);
  EXPECT_EQ(&*Sym.getCodeReferent(), &Block);
  // Setting code referent clears data referent
  EXPECT_FALSE(Sym.getDataReferent());

  Sym.setReferent(Data);
  EXPECT_EQ(&*Sym.getDataReferent(), &Data);
  // Setting data referent clears code referent
  EXPECT_FALSE(Sym.getCodeReferent());
}

TEST(Unit_Symbol, protobufRoundTrip) {
  Symbol Result;
  proto::Symbol Message;
  UUID DataUUID;

  {
    Symbol Original(Addr(1), "test");
    Original.setStorageKind(Symbol::StorageKind::Static);

    DataObject Data;
    DataUUID = Data.getUUID();
    Original.setReferent(Data);

    Original.toProtobuf(&Message);
  }

  Result.fromProtobuf(Message);

  EXPECT_EQ(Result.getAddress(), Addr(1));
  EXPECT_EQ(Result.getName(), "test");
  EXPECT_EQ(Result.getStorageKind(), Symbol::StorageKind::Static);
  EXPECT_EQ(Result.getDataReferent().getUUID(), DataUUID);
  EXPECT_EQ(Result.getCodeReferent().getUUID(), UUID());
}
