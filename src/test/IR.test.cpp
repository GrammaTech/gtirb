//===- IR.test.cpp ----------------------------------------------*- C++ -*-===//
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
#include <gtirb/AuxData.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/DataBlock.hpp>
#include <gtirb/IR.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <gtirb/proto/IR.pb.h>
#include <gtest/gtest.h>
#include <sstream>

namespace gtirb {
namespace schema {

struct TestVectorInt64 {
  static constexpr const char* Name = "test vector<int64_t>";
  typedef std::vector<int64_t> Type;
};

struct FooVectorInt64 {
  static constexpr const char* Name = "foo vector<int64_t>";
  typedef std::vector<int64_t> Type;
};

struct AnAuxDataMap {
  static constexpr const char* Name = "AuxData map";
  typedef std::map<std::string, int64_t> Type;
};

struct BarVectorChar {
  static constexpr const char* Name = "bar vector<char>";
  typedef std::vector<char> Type;
};

struct TestInt32 {
  static constexpr const char* Name = "test int32";
  typedef int32_t Type;
};

} // namespace schema
} // namespace gtirb

using namespace gtirb;
using namespace gtirb::schema;

void registerIrTestAuxDataTypes() {
  AuxDataContainer::registerAuxDataType<TestVectorInt64>();
  AuxDataContainer::registerAuxDataType<FooVectorInt64>();
  AuxDataContainer::registerAuxDataType<AnAuxDataMap>();
  AuxDataContainer::registerAuxDataType<BarVectorChar>();
  AuxDataContainer::registerAuxDataType<TestInt32>();
}

static bool hasPreferredAddr(const Module& M, Addr X) {
  return M.getPreferredAddr() == X;
}

TEST(Unit_IR, compilationIteratorTypes) {
  static_assert(std::is_same_v<IR::module_iterator::reference, Module&>);
  static_assert(
      std::is_same_v<IR::const_module_iterator::reference, const Module&>);
  // Actually calling the constructor and assignment operator tends to produce
  // more informative error messages than std::is_constructible and
  // std::is_assignable.
  IR::module_iterator It;
  IR::const_module_iterator CIt(It);
  CIt = It;
}

TEST(Unit_IR, noCopyMoveConstructors) {
  EXPECT_FALSE(std::is_copy_constructible_v<IR>);
  EXPECT_FALSE(std::is_move_constructible_v<IR>);
  EXPECT_FALSE(std::is_copy_assignable_v<IR>);
  EXPECT_FALSE(std::is_move_assignable_v<IR>);
}

static Context Ctx;
TEST(Unit_IR, ctor_0) { EXPECT_NE(IR::Create(Ctx), nullptr); }

TEST(Unit_IR, moduleIterationOrder) {
  auto* Ir = IR::Create(Ctx);
  auto* M1 = Ir->addModule(Ctx, "b");
  auto* M2 = Ir->addModule(Ctx, "a");
  auto* M3 = Ir->addModule(Ctx, "a");

  EXPECT_EQ(std::distance(Ir->modules_begin(), Ir->modules_end()), 3);
  auto It = Ir->modules_begin();
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
    auto* M = Ir->addModule(Module::Create(Ctx));
    M->setPreferredAddr(PreferredAddr);
  }

  for (size_t I = 0; I < ModulesWithoutAddr; ++I) {
    Ir->addModule(Module::Create(Ctx));
  }

  size_t Count = std::count_if(Ir->modules_begin(), Ir->modules_end(),
                               [PreferredAddr](const Module& M) {
                                 return hasPreferredAddr(M, PreferredAddr);
                               });
  EXPECT_FALSE(Count == 0);
  EXPECT_EQ(ModulesWithAddr, Count);
}

TEST(Unit_IR, addAuxData) {
  std::vector<int64_t> AuxData = {1, 2, 3};
  auto* Ir = IR::Create(Ctx);
  Ir->addAuxData<TestVectorInt64>(std::move(AuxData));

  ASSERT_NE(Ir->getAuxData<TestVectorInt64>(), nullptr);
  EXPECT_EQ(*Ir->getAuxData<TestVectorInt64>(),
            std::vector<int64_t>({1, 2, 3}));
}

TEST(Unit_IR, getAuxData) {
  std::vector<int64_t> AuxDataVec = {1, 2, 3};
  std::map<std::string, int64_t> AuxDataMap = {{"foo", 1}, {"bar", 2}};
  auto* Ir = IR::Create(Ctx);
  Ir->addAuxData<FooVectorInt64>(std::move(AuxDataVec));
  Ir->addAuxData<AnAuxDataMap>(std::move(AuxDataMap));

  auto* FooAuxData = Ir->getAuxData<FooVectorInt64>();
  ASSERT_NE(FooAuxData, nullptr);
  EXPECT_EQ(*FooAuxData, std::vector<int64_t>({1, 2, 3}));

  auto* StoredAuxDataMap = Ir->getAuxData<AnAuxDataMap>();
  std::map<std::string, int64_t> ToCompare = {{"foo", 1}, {"bar", 2}};
  std::map<std::string, int64_t> BadToCompare = {{"foo", 1}, {"bar", 3}};
  ASSERT_NE(StoredAuxDataMap, nullptr);
  EXPECT_NE(*StoredAuxDataMap, BadToCompare);
  EXPECT_EQ(*StoredAuxDataMap, ToCompare);
}

TEST(Unit_IR, auxDataRange) {
  auto* Ir = IR::Create(Ctx);
  Ir->addAuxData<FooVectorInt64>(std::vector<int64_t>{1, 2, 3});
  Ir->addAuxData<BarVectorChar>(std::vector<char>{'a', 'b', 'c'});

  auto A = Ir->aux_data();
  EXPECT_EQ(std::distance(A.begin(), A.end()), 2);
  // AuxDatas are sorted by range, but this is an implementation detail
  EXPECT_EQ(A.begin()->Key, "bar vector<char>");
  EXPECT_EQ((++A.begin())->Key, "foo vector<int64_t>");
}

TEST(Unit_IR, missingAuxData) {
  auto* Ir = IR::Create(Ctx);
  EXPECT_EQ(Ir->getAuxData<FooVectorInt64>(), nullptr);
}

TEST(Unit_IR, protobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  std::stringstream ss;
  UUID MainID;

  {
    Context InnerCtx;
    auto* Original = IR::Create(InnerCtx);
    Original->addModule(Module::Create(InnerCtx));
    Original->addAuxData<TestInt32>(42);

    MainID = Original->modules_begin()->getUUID();
    STH::save(*Original, ss);
  }
  auto* Result = *STH::load<IR>(Ctx, ss);

  EXPECT_EQ(Result->modules_begin()->getUUID(), MainID);
  EXPECT_EQ(Result->getAuxDataSize(), 1);
  ASSERT_NE(Result->getAuxData<TestInt32>(), nullptr);
  EXPECT_EQ(*Result->getAuxData<TestInt32>(), 42);
}

TEST(Unit_IR, jsonRoundTrip) {
  UUID MainID;
  std::ostringstream Out;

  {
    Context InnerCtx;
    auto* Original = IR::Create(InnerCtx);
    Original->addModule(Module::Create(InnerCtx));
    Original->addAuxData<TestInt32>(42);

    MainID = Original->modules_begin()->getUUID();
    Original->saveJSON(Out);
  }
  std::istringstream In(Out.str());
  auto* Result = *IR::loadJSON(Ctx, In);

  EXPECT_EQ(Result->modules_begin()->getUUID(), MainID);
  EXPECT_EQ(Result->getAuxDataSize(), 1);
  ASSERT_NE(Result->getAuxData<TestInt32>(), nullptr);
  EXPECT_EQ(*Result->getAuxData<TestInt32>(), 42);
}

TEST(Unit_IR, setModuleName) {
  auto* Ir = IR::Create(Ctx);
  auto* M1 = Ir->addModule(Ctx, "a");
  auto* M2 = Ir->addModule(Ctx, "b");
  auto* M3 = Ir->addModule(Ctx, "c");

  M2->setName("d");
  EXPECT_EQ(std::distance(Ir->modules_begin(), Ir->modules_end()), 3);
  auto It = Ir->modules_begin();
  EXPECT_EQ(&*It++, M1);
  EXPECT_EQ(&*It++, M3);
  EXPECT_EQ(&*It++, M2);
}

TEST(Unit_IR, CFG) {
  Module* M1 = Module::Create(Ctx, "a");
  Section* S1 = M1->addSection(Ctx, "a1");
  ByteInterval* BI1 = S1->addByteInterval(Ctx, 10);
  BI1->addBlock<CodeBlock>(Ctx, 0, 10);
  M1->addProxyBlock(Ctx);

  Module* M2 = Module::Create(Ctx, "b");
  Section* S2 = M2->addSection(Ctx, "b2");
  ByteInterval* BI2 = S2->addByteInterval(Ctx, 1);
  BI2->addBlock<DataBlock>(Ctx, 0, 1);
  M2->addProxyBlock(Ctx);

  IR* Ir = IR::Create(Ctx);
  Ir->addModule(M1);
  Ir->addModule(M2);

  {
    auto [Begin, End] = vertices(Ir->getCFG());
    ASSERT_EQ(std::distance(Begin, End), 3);
    EXPECT_EQ((std::set{Ir->getCFG()[*std::next(Begin, 0)],
                        Ir->getCFG()[*std::next(Begin, 1)],
                        Ir->getCFG()[*std::next(Begin, 2)]}),
              (std::set<CfgNode*>{&*M1->proxy_blocks_begin(),
                                  &*M1->code_blocks_begin(),
                                  &*M2->proxy_blocks_begin()}));
  }

  Ir->removeModule(M1);

  {
    auto [Begin, End] = vertices(Ir->getCFG());
    ASSERT_EQ(std::distance(Begin, End), 1);
    EXPECT_EQ(Ir->getCFG()[*Begin], &*M2->proxy_blocks_begin());
  }

  Ir->addModule(M1);

  {
    auto [Begin, End] = vertices(Ir->getCFG());
    ASSERT_EQ(std::distance(Begin, End), 3);
    EXPECT_EQ((std::set{Ir->getCFG()[*std::next(Begin, 0)],
                        Ir->getCFG()[*std::next(Begin, 1)],
                        Ir->getCFG()[*std::next(Begin, 2)]}),
              (std::set<CfgNode*>{&*M1->proxy_blocks_begin(),
                                  &*M1->code_blocks_begin(),
                                  &*M2->proxy_blocks_begin()}));
  }

  M1->removeSection(S1);

  {
    auto [Begin, End] = vertices(Ir->getCFG());
    ASSERT_EQ(std::distance(Begin, End), 2);
    EXPECT_EQ((std::set{Ir->getCFG()[*std::next(Begin, 0)],
                        Ir->getCFG()[*std::next(Begin, 1)]}),
              (std::set<CfgNode*>{&*M1->proxy_blocks_begin(),
                                  &*M2->proxy_blocks_begin()}));
  }

  BI1->addBlock<CodeBlock>(Ctx, 0, 5);
  M1->addSection(S1);

  {
    auto [Begin, End] = vertices(Ir->getCFG());
    ASSERT_EQ(std::distance(Begin, End), 4);
    EXPECT_EQ((std::set{Ir->getCFG()[*std::next(Begin, 0)],
                        Ir->getCFG()[*std::next(Begin, 1)],
                        Ir->getCFG()[*std::next(Begin, 2)],
                        Ir->getCFG()[*std::next(Begin, 3)]}),
              (std::set<CfgNode*>{&*M1->proxy_blocks_begin(),
                                  &*M1->code_blocks_begin(),
                                  &*std::next(M1->code_blocks_begin()),
                                  &*M2->proxy_blocks_begin()}));
  }

  S1->removeByteInterval(BI1);

  {
    auto [Begin, End] = vertices(Ir->getCFG());
    ASSERT_EQ(std::distance(Begin, End), 2);
    EXPECT_EQ((std::set{Ir->getCFG()[*std::next(Begin, 0)],
                        Ir->getCFG()[*std::next(Begin, 1)]}),
              (std::set<CfgNode*>{&*M1->proxy_blocks_begin(),
                                  &*M2->proxy_blocks_begin()}));
  }

  S1->addByteInterval(BI1);

  {
    auto [Begin, End] = vertices(Ir->getCFG());
    ASSERT_EQ(std::distance(Begin, End), 4);
    EXPECT_EQ((std::set{Ir->getCFG()[*std::next(Begin, 0)],
                        Ir->getCFG()[*std::next(Begin, 1)],
                        Ir->getCFG()[*std::next(Begin, 2)],
                        Ir->getCFG()[*std::next(Begin, 3)]}),
              (std::set<CfgNode*>{&*M1->proxy_blocks_begin(),
                                  &*M1->code_blocks_begin(),
                                  &*std::next(M1->code_blocks_begin()),
                                  &*M2->proxy_blocks_begin()}));
  }
}
