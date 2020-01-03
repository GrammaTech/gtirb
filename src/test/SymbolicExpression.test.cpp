//===- SymbolicExpression.test.cpp ------------------------------*- C++ -*-===//
//
//  Copyright (C) 2018-2019 GrammaTech, Inc.
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
#include <gtirb/Context.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <proto/SymbolicExpression.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

TEST(Unit_SymbolicExpression, protobufRoundTrip) {
  Symbol* Sym1 = Symbol::Create(Ctx, Addr(1), "test1");
  Symbol* Sym2 = Symbol::Create(Ctx, Addr(2), "test2");

  // SymStackConst
  {
    SymbolicExpression original(SymStackConst{1, Sym1});

    gtirb::SymbolicExpression Result;
    auto Message = toProtobuf(original);
    fromProtobuf(Ctx, Result, Message);

    SymStackConst S = std::get<SymStackConst>(Result);
    EXPECT_EQ(S.Offset, 1);
    EXPECT_EQ(S.Sym->getName(), "test1");
  }

  // SymAddrConst
  {
    SymbolicExpression original(SymAddrConst{1, Sym1});

    gtirb::SymbolicExpression Result;
    auto Message = toProtobuf(original);
    fromProtobuf(Ctx, Result, Message);

    SymAddrConst S = std::get<SymAddrConst>(Result);
    EXPECT_EQ(S.Offset, 1);
    EXPECT_EQ(S.Sym->getName(), "test1");
  }

  // SymAddrAddr
  {
    SymbolicExpression original(SymAddrAddr{1, 2, Sym1, Sym2});

    gtirb::SymbolicExpression Result;
    auto Message = toProtobuf(original);
    fromProtobuf(Ctx, Result, Message);

    SymAddrAddr S = std::get<SymAddrAddr>(Result);
    EXPECT_EQ(S.Scale, 1);
    EXPECT_EQ(S.Offset, 2);
    EXPECT_EQ(S.Sym1->getName(), "test1");
    EXPECT_EQ(S.Sym2->getName(), "test2");
  }
}
