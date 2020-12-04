//===- Symbol.test.cpp ------------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2020 GrammaTech, Inc.
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
#include "SerializationTestHarness.hpp"
#include <gtirb/CfgNode.hpp>
#include <gtirb/CodeBlock.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/DataBlock.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/ProxyBlock.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/proto/Symbol.pb.h>
#include <gtest/gtest.h>
#include <sstream>

using namespace gtirb;

static Context Ctx;

TEST(Unit_Symbol, noCopyMoveConstructors) {
  EXPECT_FALSE(std::is_copy_constructible_v<Symbol>);
  EXPECT_FALSE(std::is_move_constructible_v<Symbol>);
  EXPECT_FALSE(std::is_copy_assignable_v<Symbol>);
  EXPECT_FALSE(std::is_move_assignable_v<Symbol>);
}

TEST(Unit_Symbol, ctor_0) { EXPECT_NE(Symbol::Create(Ctx, "test"), nullptr); }

TEST(Unit_Symbol, setReferent) {
  auto* Mod = Module::Create(Ctx, "test");
  auto* Sym = Mod->addSymbol(Ctx, "test");
  auto* S = Mod->addSection(Ctx, "test");
  auto* BI = S->addByteInterval(Ctx, Addr(0), 4);
  auto* Data = BI->addBlock<DataBlock>(Ctx, 0, 2);
  auto* B = BI->addBlock<CodeBlock>(Ctx, 1, 2);
  auto* Proxy = Mod->addProxyBlock(Ctx);

  // Symbol should have no referent yet.
  EXPECT_EQ(Sym->getReferent<Node>(), nullptr);
  EXPECT_FALSE(Sym->getAddress());
  EXPECT_FALSE(Sym->hasReferent());

  Sym->setReferent(Data);
  EXPECT_EQ(Sym->getReferent<CodeBlock>(), nullptr);
  EXPECT_EQ(Sym->getReferent<DataBlock>(), Data);
  EXPECT_EQ(Sym->getReferent<ProxyBlock>(), nullptr);
  EXPECT_EQ(Sym->getAddress(), Addr(0));
  EXPECT_TRUE(Sym->hasReferent());

  Sym->setReferent(B);
  EXPECT_EQ(Sym->getReferent<CodeBlock>(), B);
  EXPECT_EQ(Sym->getReferent<DataBlock>(), nullptr);
  EXPECT_EQ(Sym->getReferent<ProxyBlock>(), nullptr);
  EXPECT_EQ(Sym->getAddress(), Addr(1));
  EXPECT_TRUE(Sym->hasReferent());

  Sym->setReferent(Proxy);
  EXPECT_EQ(Sym->getReferent<CodeBlock>(), nullptr);
  EXPECT_EQ(Sym->getReferent<DataBlock>(), nullptr);
  EXPECT_EQ(Sym->getReferent<ProxyBlock>(), Proxy);
  EXPECT_FALSE(Sym->getAddress());
  EXPECT_TRUE(Sym->hasReferent());

  Sym->setReferent<DataBlock>(nullptr);
  EXPECT_EQ(Sym->getReferent<Node>(), nullptr);
  EXPECT_FALSE(Sym->getAddress());
  EXPECT_FALSE(Sym->hasReferent());
}

TEST(Unit_Symbol, protobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  std::stringstream sym_ss;
  std::stringstream mod_ss;
  UUID DataUUID;

  // Symbol with referent
  {
    Context InnerCtx;
    auto* Mod = Module::Create(InnerCtx, "test");
    auto* Original = Symbol::Create(InnerCtx, "test");

    auto* S = Mod->addSection(InnerCtx, "test");
    auto* BI = S->addByteInterval(InnerCtx, Addr(0), 10);
    auto* Data = BI->addBlock<DataBlock>(InnerCtx, 1, 1);
    DataUUID = Data->getUUID();
    Original->setReferent(Data);

    STH::save(*Original, sym_ss);

    // We must manually serialize the symbol referent. This would typically be
    // done automatically for the user when they serialized the IR.
    STH::save(*Mod, mod_ss);
  }

  {
    Context InnerCtx;
    (void)STH::load<Module>(InnerCtx, mod_ss); // See above.
    auto* Result = STH::load<Symbol>(InnerCtx, sym_ss);

    EXPECT_EQ(Result->getAddress(), Addr(1));
    EXPECT_EQ(Result->getName(), "test");
    EXPECT_EQ(Result->getReferent<DataBlock>()->getUUID(), DataUUID);
    EXPECT_EQ(Result->getReferent<CodeBlock>(), nullptr);
  }

  // Symbol with address
  std::stringstream sym_ss2;
  {
    Context InnerCtx;
    auto* Original = Symbol::Create(InnerCtx, Addr(2), "test");
    STH::save(*Original, sym_ss2);
  }

  {
    Context InnerCtx;
    auto* Result = STH::load<Symbol>(InnerCtx, sym_ss2);

    EXPECT_EQ(Result->getAddress(), Addr(2));
    EXPECT_EQ(Result->getName(), "test");
    EXPECT_EQ(Result->getReferent<DataBlock>(), nullptr);
    EXPECT_EQ(Result->getReferent<CodeBlock>(), nullptr);
  }

  // Symbol without address
  std::stringstream sym_ss3;
  {
    Context InnerCtx;
    auto* Original = Symbol::Create(InnerCtx, "test");
    STH::save(*Original, sym_ss3);
  }

  {
    Context InnerCtx;
    auto* Result = STH::load<Symbol>(InnerCtx, sym_ss3);
    EXPECT_FALSE(Result->getAddress());
    EXPECT_EQ(Result->getName(), "test");
  }
}

TEST(Unit_Symbol, visitation) {
  auto* Sym = Symbol::Create(Ctx, CodeBlock::Create(Ctx, 2), "test");
  auto* NoRef = Symbol::Create(Ctx, "test2");

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

TEST(Unit_Symbol, atEnd) {
  auto* Mod = Module::Create(Ctx, "test");
  auto* Sym = Mod->addSymbol(Ctx, "test");
  auto* S = Mod->addSection(Ctx, "test");
  auto* BI = S->addByteInterval(Ctx, Addr(0), 4);
  auto* Data = BI->addBlock<DataBlock>(Ctx, 0, 2);
  auto* B = BI->addBlock<CodeBlock>(Ctx, 1, 2);

  Sym->setReferent(B);
  Sym->setAtEnd(true);
  EXPECT_EQ(Sym->getAddress(), gtirb::Addr{3});

  Sym->setReferent(B);
  Sym->setAtEnd(false);
  EXPECT_EQ(Sym->getAddress(), gtirb::Addr{1});

  Sym->setReferent(Data);
  Sym->setAtEnd(true);
  EXPECT_EQ(Sym->getAddress(), gtirb::Addr{2});

  Sym->setReferent(Data);
  Sym->setAtEnd(false);
  EXPECT_EQ(Sym->getAddress(), gtirb::Addr{0});
}
