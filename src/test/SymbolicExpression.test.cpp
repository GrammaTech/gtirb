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

  // SymStackConst
  {
    SymbolicExpression original(SymStackConst{1, Sym1});

    gtirb::SymbolicExpression Result;
    std::stringstream ss;
    symbolicExpressionSave(original, ss);
    symbolicExpressionLoad(Ctx, Result, ss);

    SymStackConst S = std::get<SymStackConst>(Result);
    EXPECT_EQ(S.Offset, 1);
    EXPECT_EQ(S.Sym->getName(), "test1");
    EXPECT_EQ(S.Attributes, SymAttributeSet());
  }

  // SymAddrConst
  {
    SymAttributeSet OrigSASet;
    OrigSASet.addFlags(SymAttribute::Part0, SymAttribute::Adjusted,
                       SymAttribute::PltRef);
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

TEST(Unit_SymAttributeSet, Base) {
  gtirb::SymAttributeSet SASet;

  EXPECT_EQ(SASet.begin(), SASet.end());
  for (size_t I = 0; I <= static_cast<size_t>(SymAttribute::Max); ++I) {
    EXPECT_FALSE(SASet.isFlagSet(static_cast<SymAttribute>(I)));
  }

  // Note: using Max here instead of PltRef so test continues to
  // work if more attributes are added.
  SASet.addFlags(SymAttribute::Part0, SymAttribute::Adjusted,
                 SymAttribute::Max);
  EXPECT_TRUE(SASet.isFlagSet(SymAttribute::Part0));
  EXPECT_TRUE(SASet.isFlagSet(SymAttribute::Adjusted));
  EXPECT_TRUE(SASet.isFlagSet(SymAttribute::Max));
  EXPECT_FALSE(SASet.isFlagSet(SymAttribute::Part1));

  SASet.removeFlag(SymAttribute::Part0);
  EXPECT_FALSE(SASet.isFlagSet(SymAttribute::Part0));

  SASet.addFlag(SymAttribute::Part1);
  EXPECT_TRUE(SASet.isFlagSet(SymAttribute::Part1));

  bool SawPart1 = false;
  bool SawAdj = false;
  bool SawMax = false;
  bool SawUnexpected = false;
  for (auto It = SASet.flags().begin(); It != SASet.flags().end(); ++It) {
    switch (*It) {
    case SymAttribute::Part1:
      SawPart1 = true;
      break;
    case SymAttribute::Adjusted:
      SawAdj = true;
      break;
    case SymAttribute::Max:
      SawMax = true;
      break;
    default:
      SawUnexpected = true;
    }
  }
  EXPECT_TRUE(SawPart1);
  EXPECT_TRUE(SawAdj);
  EXPECT_TRUE(SawMax);
  EXPECT_FALSE(SawUnexpected);

  SawPart1 = false;
  SawAdj = false;
  SawMax = false;
  auto It = SASet.end();
  do {
    --It;
    ASSERT_NE(It, SASet.flags().end());
    switch (*It) {
    case SymAttribute::Part1:
      SawPart1 = true;
      break;
    case SymAttribute::Adjusted:
      SawAdj = true;
      break;
    case SymAttribute::Max:
      SawMax = true;
      break;
    default:
      SawUnexpected = true;
    }
  } while (It != SASet.begin());
  EXPECT_TRUE(SawPart1);
  EXPECT_TRUE(SawAdj);
  EXPECT_TRUE(SawMax);
  EXPECT_FALSE(SawUnexpected);
}
