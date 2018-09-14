//===- Symbol.test.cpp ------------------------------------------*- C++ -*-===//
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
#include <gtirb/Symbol.hpp>
#include <proto/Symbol.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

TEST(Unit_Symbol, ctor_0) { EXPECT_NO_THROW(Symbol::Create(Ctx)); }

TEST(Unit_Symbol, setName) {
  const std::string Value{"Foo"};

  auto* Node = Symbol::Create(Ctx);
  EXPECT_NO_THROW(Node->getName());
  EXPECT_TRUE(Node->getName().empty());

  EXPECT_NO_THROW(Node->setName(Value));
  EXPECT_EQ(Value, Node->getName());
}

TEST(Unit_Symbol, setAddress) {
  const Addr Value{22678};

  auto* Node = Symbol::Create(Ctx);
  EXPECT_NO_THROW(Node->getAddress());
  EXPECT_EQ(Addr(), Node->getAddress());

  EXPECT_NO_THROW(Node->setAddress(Value));
  EXPECT_EQ(Value, Node->getAddress());
}

TEST(Unit_Symbol, setStorageKind) {
  const gtirb::Symbol::StorageKind Value{gtirb::Symbol::StorageKind::Static};

  auto* Node = Symbol::Create(Ctx);
  EXPECT_NO_THROW(Node->getStorageKind());
  EXPECT_EQ(gtirb::Symbol::StorageKind::Extern, Node->getStorageKind());

  EXPECT_NO_THROW(Node->setStorageKind(Value));
  EXPECT_EQ(Value, Node->getStorageKind());
}

TEST(Unit_Symbol, setReferent) {
  Symbol* Sym = Symbol::Create(Ctx);
  DataObject* Data = DataObject::Create(Ctx);
  Block* Block = Block::Create(Ctx);

  Sym->setReferent(*Data);
  EXPECT_EQ(Sym->getDataReferent().get(Ctx), Data);
  EXPECT_FALSE(Sym->getCodeReferent().get(Ctx));

  Sym->setReferent(*Block);
  EXPECT_EQ(Sym->getCodeReferent().get(Ctx), Block);
  // Setting code referent clears data referent
  EXPECT_FALSE(Sym->getDataReferent().get(Ctx));

  Sym->setReferent(*Data);
  EXPECT_EQ(Sym->getDataReferent().get(Ctx), Data);
  // Setting data referent clears code referent
  EXPECT_FALSE(Sym->getCodeReferent().get(Ctx));
}

TEST(Unit_Symbol, protobufRoundTrip) {
  proto::Symbol Message;
  UUID DataUUID;

  {
    Context InnerCtx;
    Symbol* Original = Symbol::Create(InnerCtx, Addr(1), "test");
    Original->setStorageKind(Symbol::StorageKind::Static);

    DataObject* Data = DataObject::Create(InnerCtx);
    DataUUID = Data->getUUID();
    Original->setReferent(*Data);

    Original->toProtobuf(&Message);
  }

  Symbol* Result = Symbol::fromProtobuf(Ctx, Message);

  EXPECT_EQ(Result->getAddress(), Addr(1));
  EXPECT_EQ(Result->getName(), "test");
  EXPECT_EQ(Result->getStorageKind(), Symbol::StorageKind::Static);
  EXPECT_EQ(Result->getDataReferent().getUUID(), DataUUID);
  EXPECT_EQ(Result->getCodeReferent().getUUID(), UUID());
}
