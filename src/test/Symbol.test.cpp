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
#include <gtirb/CfgNode.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/DataObject.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Symbol.hpp>
#include <proto/Symbol.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

TEST(Unit_Symbol, ctor_0) { EXPECT_NE(Symbol::Create(Ctx), nullptr); }

TEST(Unit_Symbol, setStorageKind) {
  const gtirb::Symbol::StorageKind Value{gtirb::Symbol::StorageKind::Static};

  auto* Node = Symbol::Create(Ctx);
  EXPECT_EQ(gtirb::Symbol::StorageKind::Extern, Node->getStorageKind());

  Node->setStorageKind(Value);
  EXPECT_EQ(Value, Node->getStorageKind());
}

TEST(Unit_Symbol, setReferent) {
  Module* Mod = Module::Create(Ctx);
  Symbol* Sym = emplaceSymbol(*Mod, Ctx);
  DataObject* Data = DataObject::Create(Ctx);
  Mod->addData(Data);
  Block* B = emplaceBlock(*Mod, Ctx, Addr(1), 2);

  // Symbol should have no referent yet.
  EXPECT_EQ(Sym->getReferent<Node>(), nullptr);
  EXPECT_FALSE(Sym->getAddress());

  setReferent(*Mod, *Sym, Data);
  EXPECT_EQ(Sym->getReferent<DataObject>(), Data);
  EXPECT_EQ(Sym->getReferent<Block>(), nullptr);
  EXPECT_EQ(Sym->getAddress(), Addr(0));

  setReferent(*Mod, *Sym, B);
  EXPECT_EQ(Sym->getReferent<Block>(), B);
  EXPECT_EQ(Sym->getReferent<DataObject>(), nullptr);
  EXPECT_EQ(Sym->getAddress(), Addr(1));
}

TEST(Unit_Symbol, protobufRoundTrip) {
  proto::Symbol SMessage;
  proto::DataObject DOMessage;
  UUID DataUUID;

  // Symbol with referent
  {
    Context InnerCtx;
    Module* Mod = Module::Create(Ctx);
    Symbol* Original = emplaceSymbol(*Mod, InnerCtx, "test");
    Original->setStorageKind(Symbol::StorageKind::Static);

    DataObject* Data = DataObject::Create(InnerCtx, Addr(1), 1);
    DataUUID = Data->getUUID();
    setReferent(*Mod, *Original, Data);

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

  // Symbol with address
  {
    Context InnerCtx;
    Symbol* Original = Symbol::Create(InnerCtx, Addr(2), "test");
    Original->toProtobuf(&SMessage);
  }
  Result = Symbol::fromProtobuf(Ctx, SMessage);

  EXPECT_EQ(Result->getAddress(), Addr(2));
  EXPECT_EQ(Result->getName(), "test");
  EXPECT_EQ(Result->getStorageKind(), Symbol::StorageKind::Extern);
  EXPECT_EQ(Result->getReferent<DataObject>(), nullptr);
  EXPECT_EQ(Result->getReferent<Block>(), nullptr);

  // Symbol without address
  {
    Context InnerCtx;
    Symbol* Original = Symbol::Create(InnerCtx, "test");
    Original->toProtobuf(&SMessage);
  }
  Result = Symbol::fromProtobuf(Ctx, SMessage);
  EXPECT_FALSE(Result->getAddress());
  EXPECT_EQ(Result->getName(), "test");
}

TEST(Unit_Symbol, visitation) {
  Symbol* Sym = Symbol::Create(Ctx, Block::Create(Ctx, Addr(1), 2), "test");
  Symbol* NoRef = Symbol::Create(Ctx, Addr(1), "test2");

  struct Visitor {
    int operator()(Block* B) {
      // This should not be called with a null pointer.
      EXPECT_NE(B, nullptr);
      return 0;
    }
    long operator()(DataObject*) {
      // This overload should never be called.
      EXPECT_TRUE(false);
      return 1;
    }
    long operator()(ProxyBlock*) {
      // This overload should never be called.
      EXPECT_TRUE(false);
      return 1;
    }
  };
  EXPECT_EQ(0, *Sym->visit(Visitor{}));

  // The version that has no referent should not call any of the visitor
  // functions and the returned optional should not have a value.
  struct NoRefVisitor {
    int operator()(const CfgNode*) const {
      EXPECT_TRUE(false);
      return 0;
    }
    int operator()(const DataObject*) const {
      EXPECT_TRUE(false);
      return 1;
    }
  };
  EXPECT_FALSE(NoRef->visit(NoRefVisitor{}));

  // Similar to the test above, but ensuring we can visit without a return type.
  struct ConstVoidVisitor {
    void operator()(const CfgNode* N) const { EXPECT_NE(N, nullptr); }
    void operator()(const DataObject*) const { EXPECT_TRUE(false); }
  };
  Sym->visit(ConstVoidVisitor{});

  // Ensure that we can provide a visitor that uses a base type.
  struct GenericVisitor {
    void operator()(const Node* N) const {
      EXPECT_NE(N, nullptr);
      // This should still only be called once.
      static int Counter;
      EXPECT_EQ(Counter++, 0);
    }
  };
  Sym->visit(GenericVisitor{});

  // Ensure that we can provide a lambda as a callable.
  Sym->visit([](const Node* N) {
    EXPECT_NE(N, nullptr);
    // This should still only be called once.
    static int Counter;
    EXPECT_EQ(Counter++, 0);
  });

  // The following is example code that should not compile. We cannot use gtest
  // to ensure that we get the appropriate compile errors, unfortunately.
  // struct NotEnoughOverloads {
  //  void operator()(const Block*) {}
  //};
  // Sym->visit(NotEnoughOverloads{}); // Error

  // struct IncorrectReturnTypes {
  //  std::string operator()(Block*) { return ""; }
  //  int operator()(DataObject*) { return 0; }
  //};
  // Sym->visit(IncorrectReturnTypes{}); // Error
}
