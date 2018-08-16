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
    EXPECT_NO_THROW(Ir->getModules().push_back(M));
  }

  for (size_t I = 0; I < ModulesWithoutAddr; ++I) {
    Module *M = Module::Create(Ctx);
    EXPECT_NO_THROW(Ir->getModules().push_back(M));
  }

  const auto Modules = Ir->getModulesWithPreferredAddr(PreferredAddr);
  EXPECT_FALSE(Modules.empty());
  EXPECT_EQ(ModulesWithAddr, Modules.size());
}

TEST(Unit_IR, getModulesContainingAddr) {
  const Addr Ea{22678};
  const uint64_t EaOffset{2112};

  auto *Ir = IR::Create(Ctx);

  // Addr at lower bound
  {
    Module *M = Module::Create(Ctx);
    M->getImageByteMap().setAddrMinMax({Ea, Ea + EaOffset});
    EXPECT_NO_THROW(Ir->getModules().push_back(M));
  }

  // Addr inside range
  {
    Module *M = Module::Create(Ctx);
    M->getImageByteMap().setAddrMinMax({Ea - EaOffset, Ea + EaOffset});
    EXPECT_NO_THROW(Ir->getModules().push_back(M));
  }

  // Addr at max (should not be returned)
  {
    Module *M = Module::Create(Ctx);
    M->getImageByteMap().setAddrMinMax({Ea - EaOffset, Ea});
    EXPECT_NO_THROW(Ir->getModules().push_back(M));
  }

  const auto modules = Ir->getModulesContainingAddr(Ea);
  EXPECT_FALSE(modules.empty());
  EXPECT_EQ(size_t(2), modules.size());
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
    IR *Original = IR::Create(Ctx);
    Module *M = Module::Create(Ctx);
    M->getImageByteMap().setAddrMinMax({Addr(100), Addr(200)});
    Original->getModules().push_back(M);
    Original->addTable("test", Table());

    MainID = Original->getModules().front()->getUUID();
    Original->toProtobuf(&Message);
    details::ClearUUIDs(Original); // Avoid UUID conflict
  }
  IR *Result = IR::fromProtobuf(Ctx, Message);

  EXPECT_EQ(Result->getModules().front()->getUUID(), MainID);
  EXPECT_EQ(Result->getModulesContainingAddr(Addr(100)).size(), 1);
  EXPECT_EQ(Result->getTableSize(), 1);
  EXPECT_NE(Result->getTable("test"), nullptr);
}
