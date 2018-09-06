#include <gtirb/Context.hpp>
#include <gtirb/DataObject.hpp>
#include <gtirb/IR.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <gtirb/Table.hpp>
#include <proto/IR.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;
TEST(Unit_IR, ctor_0) { EXPECT_NO_THROW(IR::Create(Ctx)); }

TEST(Unit_IR, getModulesWithPreferredAddr) {
  const Addr PreferredAddr{22678};
  const size_t ModulesWithAddr{3};
  const size_t ModulesWithoutAddr{5};

  auto *Ir = IR::Create(Ctx);

  for (size_t I = 0; I < ModulesWithAddr; ++I) {
    Module *M = Module::Create(Ctx);
    M->setPreferredAddr(PreferredAddr);
    EXPECT_NO_THROW(Ir->addModule(M));
  }

  for (size_t I = 0; I < ModulesWithoutAddr; ++I) {
    Module *M = Module::Create(Ctx);
    EXPECT_NO_THROW(Ir->addModule(M));
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

  auto *Ir = IR::Create(Ctx);

  // Addr at lower bound
  {
    Module *M = Module::Create(Ctx);
    M->getImageByteMap().setAddrMinMax({Ea, Ea + EaOffset});
    EXPECT_NO_THROW(Ir->addModule(M));
  }

  // Addr inside range
  {
    Module *M = Module::Create(Ctx);
    M->getImageByteMap().setAddrMinMax({Ea - EaOffset, Ea + EaOffset});
    EXPECT_NO_THROW(Ir->addModule(M));
  }

  // Addr at max (should not be returned)
  {
    Module *M = Module::Create(Ctx);
    M->getImageByteMap().setAddrMinMax({Ea - EaOffset, Ea});
    EXPECT_NO_THROW(Ir->addModule(M));
  }

  size_t Count =
    std::count_if(Ir->begin(), Ir->end(), [Ea](const Module& M) {
    return containsAddr(M, Ea);
  });
  EXPECT_FALSE(Count == 0);
  EXPECT_EQ(2, Count);
}

TEST(Unit_IR, addTable) {
  std::vector<int64_t> Table = {1, 2, 3};
  IR *Ir = IR::Create(Ctx);
  Ir->addTable("test", std::move(Table));

  EXPECT_NE(Ir->getTable("test"), nullptr);
  EXPECT_EQ(std::get<std::vector<int64_t>>(*Ir->getTable("test")),
            std::vector<int64_t>({1, 2, 3}));
}

TEST(Unit_IR, missingTable) {
  IR *Ir = IR::Create(Ctx);
  EXPECT_EQ(Ir->getTable("missing"), nullptr);
}

TEST(Unit_IR, protobufRoundTrip) {
  proto::IR Message;
  UUID MainID;

  {
    Context InnerCtx;
    IR *Original = IR::Create(InnerCtx);
    Module *M = Module::Create(InnerCtx);
    M->getImageByteMap().setAddrMinMax({Addr(100), Addr(200)});
    Original->addModule(M);
    Original->addTable("test", Table());

    MainID = Original->begin()->getUUID();
    Original->toProtobuf(&Message);
  }
  IR *Result = IR::fromProtobuf(Ctx, Message);

  EXPECT_EQ(Result->begin()->getUUID(), MainID);
  size_t Count =
      std::count_if(Result->begin(), Result->end(),
                    [](const Module& M) { return containsAddr(M, Addr(100)); });
  EXPECT_EQ(Count, 1);
  EXPECT_EQ(Result->getTableSize(), 1);
  EXPECT_NE(Result->getTable("test"), nullptr);
}
