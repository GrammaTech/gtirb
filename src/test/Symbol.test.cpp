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
#include <gtirb/Module.hpp>
#include <gtirb/Symbol.hpp>
#include <proto/Symbol.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

TEST(Unit_Symbol, ctor_0) { EXPECT_NE(Symbol::Create(Ctx), nullptr); }

TEST(Unit_Symbol, setName) {
  const std::string Value{"Foo"};

  auto* Node = Symbol::Create(Ctx);
  EXPECT_TRUE(Node->getName().empty());

  Node->setName(Value);
  EXPECT_EQ(Value, Node->getName());
}

TEST(Unit_Symbol, setAddress) {
  const Addr Value{22678};

  auto* Node = Symbol::Create(Ctx);
  EXPECT_EQ(Addr(), Node->getAddress());

  Node->setAddress(Value);
  EXPECT_EQ(Value, Node->getAddress());
}

TEST(Unit_Symbol, setStorageKind) {
  const gtirb::Symbol::StorageKind Value{gtirb::Symbol::StorageKind::Static};

  auto* Node = Symbol::Create(Ctx);
  EXPECT_EQ(gtirb::Symbol::StorageKind::Extern, Node->getStorageKind());

  Node->setStorageKind(Value);
  EXPECT_EQ(Value, Node->getStorageKind());
}

TEST(Unit_Symbol, setReferent) {
  Symbol* Sym = Symbol::Create(Ctx);
  DataObject* Data = DataObject::Create(Ctx);
  Block* B = Block::Create(Ctx);

  // Symbol should have no referent yet.
  EXPECT_EQ(Sym->getReferent<Node>(), nullptr);

  Sym->setReferent(Data);
  EXPECT_EQ(Sym->getReferent<DataObject>(), Data);
  EXPECT_EQ(Sym->getReferent<Block>(), nullptr);

  Sym->setReferent(B);
  EXPECT_EQ(Sym->getReferent<Block>(), B);
  EXPECT_EQ(Sym->getReferent<DataObject>(), nullptr);
}

TEST(Unit_Symbol, protobufRoundTrip) {
  proto::Symbol SMessage;
  proto::DataObject DOMessage;
  UUID DataUUID;

  {
    Context InnerCtx;
    Symbol* Original = Symbol::Create(InnerCtx, Addr(1), "test");
    Original->setStorageKind(Symbol::StorageKind::Static);

    DataObject* Data = DataObject::Create(InnerCtx);
    DataUUID = Data->getUUID();
    Original->setReferent(Data);

    Original->toProtobuf(&SMessage);

    // We must manually serialize the symbol referent. This would typically be
    // done automatically for the user when they serialized the IR.
    Data->toProtobuf(&DOMessage);
  }

  (void)DataObject::fromProtobuf(Ctx, DOMessage); // See above.
  Symbol* Result = Symbol::fromProtobuf(Ctx, SMessage);

  EXPECT_EQ(Result->getAddress(), Addr(1));
  EXPECT_EQ(Result->getName(), "test");
  EXPECT_EQ(Result->getStorageKind(), Symbol::StorageKind::Static);
  EXPECT_EQ(Result->getReferent<DataObject>()->getUUID(), DataUUID);
  EXPECT_EQ(Result->getReferent<Block>(), nullptr);
}

TEST(Unit_Symbol, visitation) {
  struct Visitor {
    int operator()(Block*) { return 0; }
    long operator()(DataObject*) { return 1; }
  };

  struct BadVisitor {
    void operator()(const Block*) {}
  };

  struct ConstVoidVisitor {
    void operator()(const Block*) const {}
    void operator()(const DataObject*) const {}
  };

  Symbol* Sym = Symbol::Create(Ctx, Addr(1), "test", Block::Create(Ctx));
  (void)Sym->visit(Visitor{});
  //Sym->visit(BadVisitor{}); // Error
  Sym->visit(ConstVoidVisitor{});
}
