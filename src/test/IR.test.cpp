//===- IR.test.cpp ----------------------------------------------*- C++ -*-===//
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
#include <gtirb/AuxData.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/DataBlock.hpp>
#include <gtirb/IR.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <proto/IR.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

static bool hasPreferredAddr(const Module& M, Addr X) {
  return M.getPreferredAddr() == X;
}

TEST(Unit_IR, compilationIteratorTypes) {
  static_assert(std::is_same_v<IR::iterator::reference, Module&>);
  static_assert(std::is_same_v<IR::const_iterator::reference, const Module&>);
  // Actually calling the constructor and assignment operator tends to produce
  // more informative error messages than std::is_constructible and
  // std::is_assignable.
  IR::iterator It;
  IR::const_iterator CIt(It);
  CIt = It;
}

static Context Ctx;
TEST(Unit_IR, ctor_0) { EXPECT_NE(IR::Create(Ctx), nullptr); }

TEST(Unit_IR, moduleIterationOrder) {
  auto* Ir = IR::Create(Ctx);
  auto* M1 = Ir->addModule(Ctx, "b");
  auto* M2 = Ir->addModule(Ctx, "a");
  auto* M3 = Ir->addModule(Ctx, "a");

  EXPECT_EQ(std::distance(Ir->begin(), Ir->end()), 3);
  auto It = Ir->begin();
  // Order of M2 and M3 is unspecified.
  if (&*It == M2) {
    ++It;
    EXPECT_EQ(&*It, M3);
  } else {
    EXPECT_EQ(&*It, M3);
    ++It;
    EXPECT_EQ(&*It, M2);
  }
  ++It;
  EXPECT_EQ(&*It, M1);
}

TEST(Unit_IR, getModulesWithPreferredAddr) {
  const Addr PreferredAddr{22678};
  const size_t ModulesWithAddr{3};
  const size_t ModulesWithoutAddr{5};

  auto* Ir = IR::Create(Ctx);

  for (size_t I = 0; I < ModulesWithAddr; ++I) {
    auto* M = Ir->addModule(Ctx);
    M->setPreferredAddr(PreferredAddr);
  }

  for (size_t I = 0; I < ModulesWithoutAddr; ++I) {
    Ir->addModule(Ctx);
  }

  size_t Count =
      std::count_if(Ir->begin(), Ir->end(), [PreferredAddr](const Module& M) {
        return hasPreferredAddr(M, PreferredAddr);
      });
  EXPECT_FALSE(Count == 0);
  EXPECT_EQ(ModulesWithAddr, Count);
}

TEST(Unit_IR, addAuxData) {
  std::vector<int64_t> AuxData = {1, 2, 3};
  auto* Ir = IR::Create(Ctx);
  Ir->addAuxData("test", std::move(AuxData));

  EXPECT_NE(Ir->getAuxData("test"), nullptr);
  EXPECT_EQ(*Ir->getAuxData("test")->get<std::vector<int64_t>>(),
            std::vector<int64_t>({1, 2, 3}));
}

TEST(Unit_IR, getAuxData) {
  std::vector<int64_t> AuxDataVec = {1, 2, 3};
  std::map<std::string, int64_t> AuxDataMap = {{"foo", 1}, {"bar", 2}};
  auto* Ir = IR::Create(Ctx);
  Ir->addAuxData("foo", std::move(AuxDataVec));
  Ir->addAuxData("bar", std::move(AuxDataMap));

  auto* FooAuxData = Ir->getAuxData<std::vector<int64_t>>("foo");
  EXPECT_NE(FooAuxData, nullptr);
  EXPECT_EQ(*FooAuxData, std::vector<int64_t>({1, 2, 3}));

  auto* BarAuxData = Ir->getAuxData<std::map<std::string, int64_t>>("bar");
  std::map<std::string, int64_t> ToCompare = {{"foo", 1}, {"bar", 2}};
  std::map<std::string, int64_t> BadToCompare = {{"foo", 1}, {"bar", 3}};
  EXPECT_NE(BarAuxData, nullptr);
  EXPECT_NE(*BarAuxData, BadToCompare);
  EXPECT_EQ(*BarAuxData, ToCompare);
}

TEST(Unit_IR, auxDataRange) {
  auto* Ir = IR::Create(Ctx);
  Ir->addAuxData("foo", std::vector<int64_t>{1, 2, 3});
  Ir->addAuxData("bar", std::vector<char>{'a', 'b', 'c'});

  auto A = Ir->aux_data();
  EXPECT_EQ(std::distance(A.begin(), A.end()), 2);
  // AuxDatas are sorted by range, but this is an implementation detail
  EXPECT_EQ(A.begin()->first, "bar");
  EXPECT_EQ((++A.begin())->first, "foo");
}

TEST(Unit_IR, missingAuxData) {
  auto* Ir = IR::Create(Ctx);
  EXPECT_EQ(Ir->getAuxData("missing"), nullptr);
}

TEST(Unit_IR, protobufRoundTrip) {
  proto::IR Message;
  UUID MainID;

  {
    Context InnerCtx;
    auto* Original = IR::Create(InnerCtx);
    Original->addModule(InnerCtx);
    Original->addAuxData("test", AuxData());

    MainID = Original->begin()->getUUID();
    Original->toProtobuf(&Message);
  }
  auto* Result = IR::fromProtobuf(Ctx, Message);

  EXPECT_EQ(Result->begin()->getUUID(), MainID);
  EXPECT_EQ(Result->getAuxDataSize(), 1);
  EXPECT_NE(Result->getAuxData("test"), nullptr);
}

TEST(Unit_IR, jsonRoundTrip) {
  UUID MainID;
  std::ostringstream Out;

  {
    Context InnerCtx;
    auto* Original = IR::Create(InnerCtx);
    Original->addModule(InnerCtx);
    Original->addAuxData("test", AuxData());

    MainID = Original->begin()->getUUID();
    Original->saveJSON(Out);
  }
  std::istringstream In(Out.str());
  auto* Result = IR::loadJSON(Ctx, In);

  EXPECT_EQ(Result->begin()->getUUID(), MainID);
  EXPECT_EQ(Result->getAuxDataSize(), 1);
  EXPECT_NE(Result->getAuxData("test"), nullptr);
}

TEST(Unit_IR, move) {
  auto* Original = IR::Create(Ctx);
  EXPECT_TRUE(Original->getAuxDataEmpty());

  Original->addAuxData("test", AuxData());

  IR Moved(std::move(*Original));
  EXPECT_FALSE(Moved.getAuxDataEmpty());
  EXPECT_EQ(Moved.getAuxDataSize(), 1);
  EXPECT_NE(Moved.getAuxData("test"), nullptr);
}

TEST(Unit_IR, setModuleName) {
  auto* Ir = IR::Create(Ctx);
  auto* M1 = Ir->addModule(Ctx, "a");
  auto* M2 = Ir->addModule(Ctx, "b");
  auto* M3 = Ir->addModule(Ctx, "c");

  M2->setName("d");
  EXPECT_EQ(std::distance(Ir->begin(), Ir->end()), 3);
  auto It = Ir->begin();
  EXPECT_EQ(&*It++, M1);
  EXPECT_EQ(&*It++, M3);
  EXPECT_EQ(&*It++, M2);
}
