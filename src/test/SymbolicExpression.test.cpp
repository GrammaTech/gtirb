//===- SymbolicExpression.test.cpp ------------------------------*- C++ -*-===//
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
#include <gtirb/Context.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <gtirb/proto/SymbolicExpression.pb.h>
#include <gtest/gtest.h>
#include <sstream>

using namespace gtirb;

static Context Ctx;

TEST(Unit_SymbolicExpression, protobufRoundTrip) {
  Symbol* Sym1 = Symbol::Create(Ctx, Addr(1), "test1");
  Symbol* Sym2 = Symbol::Create(Ctx, Addr(2), "test2");

  // SymAddrConst
  {
    SymAttributeSet OrigSASet;
    OrigSASet.insert({SymAttribute::ABS, SymAttribute::G0});
    SymbolicExpression original(SymAddrConst{1, Sym1, OrigSASet});

    gtirb::SymbolicExpression Result;
    std::stringstream ss;
    symbolicExpressionSave(original, ss);
    symbolicExpressionLoad(Ctx, Result, ss);

    SymAddrConst S = std::get<SymAddrConst>(Result);
    EXPECT_EQ(S.Offset, 1);
    EXPECT_EQ(S.Sym->getName(), "test1");
    EXPECT_EQ(S.Attributes, OrigSASet);
  }

  // SymAddrAddr
  {
    SymbolicExpression original(SymAddrAddr{1, 2, Sym1, Sym2});

    gtirb::SymbolicExpression Result;
    std::stringstream ss;
    symbolicExpressionSave(original, ss);
    symbolicExpressionLoad(Ctx, Result, ss);

    SymAddrAddr S = std::get<SymAddrAddr>(Result);
    EXPECT_EQ(S.Scale, 1);
    EXPECT_EQ(S.Offset, 2);
    EXPECT_EQ(S.Sym1->getName(), "test1");
    EXPECT_EQ(S.Sym2->getName(), "test2");
  }
}

TEST(Unit_SymAttributeSet, unknownAttributes) {
  std::ostringstream Out;
  {
    auto* I = IR::Create(Ctx);
    auto* M = I->addModule(Ctx, "foo");
    auto* S = M->addSection(Ctx, "bar");
    auto* B = S->addByteInterval(Ctx, Addr(0), 8);

    Symbol* Sym = Symbol::Create(Ctx, Addr(4), "baz");
    SymAttributeSet Attrs{SymAttribute::GOT, static_cast<SymAttribute>(0xBEEF)};
    B->addSymbolicExpression<SymAddrConst>(4, SymAddrConst{0, Sym, Attrs});

    I->save(Out);
  }

  std::istringstream In(Out.str());
  {
    auto ResultOrErr = IR::load(Ctx, In);
    ASSERT_TRUE(ResultOrErr);

    auto* I = *ResultOrErr;
    EXPECT_EQ(std::distance(I->symbolic_expressions_begin(),
                            I->symbolic_expressions_end()),
              1);
    EXPECT_TRUE(std::holds_alternative<SymAddrConst>(
        I->symbolic_expressions_begin()->getSymbolicExpression()));

    SymAttributeSet Attrs =
        std::get<SymAddrConst>(
            I->symbolic_expressions_begin()->getSymbolicExpression())
            .Attributes;
    EXPECT_EQ(Attrs.count(SymAttribute::GOT), 1);
    EXPECT_EQ(Attrs.count(static_cast<SymAttribute>(0xBEEF)), 1);
  }
}
