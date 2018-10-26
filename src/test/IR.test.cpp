//===- IR.test.cpp ----------------------------------------------*- C++ -*-===//
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
#include <gtirb/AuxData.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/DataObject.hpp>
#include <gtirb/IR.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <proto/IR.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;
TEST(Unit_IR, ctor_0) { EXPECT_NE(IR::Create(Ctx), nullptr); }

TEST(Unit_IR, getModulesWithPreferredAddr) {
  const Addr PreferredAddr{22678};
  const size_t ModulesWithAddr{3};
  const size_t ModulesWithoutAddr{5};

  auto* Ir = IR::Create(Ctx);

  for (size_t I = 0; I < ModulesWithAddr; ++I) {
    Module* M = Module::Create(Ctx);
    M->setPreferredAddr(PreferredAddr);
    Ir->addModule(M);
  }

  for (size_t I = 0; I < ModulesWithoutAddr; ++I) {
    Module* M = Module::Create(Ctx);
    Ir->addModule(M);
  }

  size_t Count =
      std::count_if(Ir->begin(), Ir->end(), [PreferredAddr](const Module& M) {
        return hasPreferredAddr(M, PreferredAddr);
      });
  EXPECT_FALSE(Count == 0);
  EXPECT_EQ(ModulesWithAddr, Count);
}

TEST(Unit_IR, getModulesContainingAddr) {
  const Addr Ea{22678};
  const uint64_t EaOffset{2112};

  auto* Ir = IR::Create(Ctx);

  // Addr at lower bound
  {
    Module* M = Module::Create(Ctx);
    M->getImageByteMap().setAddrMinMax({Ea, Ea + EaOffset});
    Ir->addModule(M);
  }

  // Addr inside range
  {
    Module* M = Module::Create(Ctx);
    M->getImageByteMap().setAddrMinMax({Ea - EaOffset, Ea + EaOffset});
    Ir->addModule(M);
  }

  // Addr at max (should not be returned)
  {
    Module* M = Module::Create(Ctx);
    M->getImageByteMap().setAddrMinMax({Ea - EaOffset, Ea});
    Ir->addModule(M);
  }

  size_t Count = std::count_if(Ir->begin(), Ir->end(), [Ea](const Module& M) {
    return containsAddr(M, Ea);
  });
  EXPECT_FALSE(Count == 0);
  EXPECT_EQ(2, Count);
}

TEST(Unit_IR, addAuxData) {
  std::vector<int64_t> AuxData = {1, 2, 3};
  IR* Ir = IR::Create(Ctx);
  Ir->addAuxData("test", std::move(AuxData));

  EXPECT_NE(Ir->getAuxData("test"), nullptr);
  EXPECT_EQ(*Ir->getAuxData("test")->get<std::vector<int64_t>>(),
            std::vector<int64_t>({1, 2, 3}));
}

TEST(Unit_IR, auxDataRange) {
  IR* Ir = IR::Create(Ctx);
  Ir->addAuxData("foo", std::vector<int64_t>{1, 2, 3});
  Ir->addAuxData("bar", std::vector<char>{'a', 'b', 'c'});

  auto A = Ir->aux_data();
  EXPECT_EQ(std::distance(A.begin(), A.end()), 2);
  // AuxDatas are sorted by range, but this is an implementation detail
  EXPECT_EQ(A.begin()->first, "bar");
  EXPECT_EQ((++A.begin())->first, "foo");
}

TEST(Unit_IR, missingAuxData) {
  IR* Ir = IR::Create(Ctx);
  EXPECT_EQ(Ir->getAuxData("missing"), nullptr);
}

TEST(Unit_IR, protobufRoundTrip) {
  proto::IR Message;
  UUID MainID;

  {
    Context InnerCtx;
    IR* Original = IR::Create(InnerCtx);
    Module* M = Module::Create(InnerCtx);
    M->getImageByteMap().setAddrMinMax({Addr(100), Addr(200)});
    Original->addModule(M);
    Original->addAuxData("test", AuxData());

    MainID = Original->begin()->getUUID();
    Original->toProtobuf(&Message);
  }
  IR* Result = IR::fromProtobuf(Ctx, Message);

  EXPECT_EQ(Result->begin()->getUUID(), MainID);
  size_t Count =
      std::count_if(Result->begin(), Result->end(),
                    [](const Module& M) { return containsAddr(M, Addr(100)); });
  EXPECT_EQ(Count, 1);
  EXPECT_EQ(Result->getAuxDataSize(), 1);
  EXPECT_NE(Result->getAuxData("test"), nullptr);
}

TEST(Unit_IR, jsonRoundTrip) {
  UUID MainID;
  std::ostringstream Out;

  {
    Context InnerCtx;
    IR* Original = IR::Create(InnerCtx);
    Module* M = Module::Create(InnerCtx);
    M->getImageByteMap().setAddrMinMax({Addr(100), Addr(200)});
    Original->addModule(M);
    Original->addAuxData("test", AuxData());

    MainID = Original->begin()->getUUID();
    Original->saveJSON(Out);
  }
  std::istringstream In(Out.str());
  IR* Result = IR::loadJSON(Ctx, In);

  EXPECT_EQ(Result->begin()->getUUID(), MainID);
  size_t Count =
      std::count_if(Result->begin(), Result->end(),
                    [](const Module& M) { return containsAddr(M, Addr(100)); });
  EXPECT_EQ(Count, 1);
  EXPECT_EQ(Result->getAuxDataSize(), 1);
  EXPECT_NE(Result->getAuxData("test"), nullptr);
}

TEST(Unit_IR, move) {
  IR* Original = IR::Create(Ctx);
  EXPECT_TRUE(Original->getAuxDataEmpty());

  Original->addAuxData("test", AuxData());

  IR Moved(std::move(*Original));
  EXPECT_FALSE(Moved.getAuxDataEmpty());
  EXPECT_EQ(Moved.getAuxDataSize(), 1);
  EXPECT_NE(Moved.getAuxData("test"), nullptr);
}
