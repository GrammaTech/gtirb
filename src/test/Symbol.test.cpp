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
#include <gtirb/CfgNode.hpp>
#include <gtirb/CodeBlock.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/DataBlock.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/ProxyBlock.hpp>
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
  DataBlock* Data = DataBlock::Create(Ctx);
  CodeBlock* B = CodeBlock::Create(Ctx, 2);
  ProxyBlock* Proxy = ProxyBlock::Create(Ctx);
  Mod->addProxyBlock(Proxy);

  // Symbol should have no referent yet.
  EXPECT_EQ(Sym->getReferent<Node>(), nullptr);
  EXPECT_FALSE(Sym->getAddress());

  setReferent(*Mod, *Sym, Data);
  EXPECT_EQ(Sym->getReferent<CodeBlock>(), nullptr);
  EXPECT_EQ(Sym->getReferent<DataBlock>(), Data);
  EXPECT_EQ(Sym->getReferent<ProxyBlock>(), nullptr);
  EXPECT_EQ(Sym->getAddress(), Addr(0));

  setReferent(*Mod, *Sym, B);
  EXPECT_EQ(Sym->getReferent<CodeBlock>(), B);
  EXPECT_EQ(Sym->getReferent<DataBlock>(), nullptr);
  EXPECT_EQ(Sym->getReferent<ProxyBlock>(), nullptr);
  EXPECT_EQ(Sym->getAddress(), Addr(1));

  setReferent(*Mod, *Sym, Proxy);
  EXPECT_EQ(Sym->getReferent<CodeBlock>(), nullptr);
  EXPECT_EQ(Sym->getReferent<DataBlock>(), nullptr);
  EXPECT_EQ(Sym->getReferent<ProxyBlock>(), Proxy);
  EXPECT_FALSE(Sym->getAddress());
}

TEST(Unit_Symbol, protobufRoundTrip) {
  proto::Symbol SMessage;
  proto::DataBlock DOMessage;
  UUID DataUUID;

  // Symbol with referent
  {
    Context InnerCtx;
    Module* Mod = Module::Create(Ctx);
    Symbol* Original = emplaceSymbol(*Mod, InnerCtx, "test");
    Original->setStorageKind(Symbol::StorageKind::Static);

    DataBlock* Data = DataBlock::Create(InnerCtx, 1);
    DataUUID = Data->getUUID();
    setReferent(*Mod, *Original, Data);

    Original->toProtobuf(&SMessage);

    // We must manually serialize the symbol referent. This would typically be
    // done automatically for the user when they serialized the IR.
    Data->toProtobuf(&DOMessage);
  }

  (void)DataBlock::fromProtobuf(Ctx, DOMessage); // See above.
  Symbol* Result = Symbol::fromProtobuf(Ctx, SMessage);

  EXPECT_EQ(Result->getAddress(), Addr(1));
  EXPECT_EQ(Result->getName(), "test");
  EXPECT_EQ(Result->getStorageKind(), Symbol::StorageKind::Static);
  EXPECT_EQ(Result->getReferent<DataBlock>()->getUUID(), DataUUID);
  EXPECT_EQ(Result->getReferent<CodeBlock>(), nullptr);

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
  EXPECT_EQ(Result->getReferent<DataBlock>(), nullptr);
  EXPECT_EQ(Result->getReferent<CodeBlock>(), nullptr);

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
  Symbol* Sym = Symbol::Create(Ctx, CodeBlock::Create(Ctx, 2), "test");
  Symbol* NoRef = Symbol::Create(Ctx, "test2");

  struct Visitor {
    int operator()(CodeBlock* B) {
      // This should not be called with a null pointer.
      EXPECT_NE(B, nullptr);
      return 0;
    }
    long operator()(DataBlock*) {
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
    int operator()(const DataBlock*) const {
      EXPECT_TRUE(false);
      return 1;
    }
  };
  EXPECT_FALSE(NoRef->visit(NoRefVisitor{}));

  // Similar to the test above, but ensuring we can visit without a return type.
  struct ConstVoidVisitor {
    void operator()(const CfgNode* N) const { EXPECT_NE(N, nullptr); }
    void operator()(const DataBlock*) const { EXPECT_TRUE(false); }
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
  //  int operator()(DataBlock*) { return 0; }
  //};
  // Sym->visit(IncorrectReturnTypes{}); // Error
}
