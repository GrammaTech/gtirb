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
#include <memory>

using namespace gtirb;

static Context Ctx;

TEST(Unit_IR, ctor_0) { EXPECT_NO_THROW(IR::Create(Ctx)); }

TEST(Unit_IR, getModulesWithPreferredEA) {
  const gtirb::EA PreferredEA{22678};
  const size_t ModulesWithEA{3};
  const size_t ModulesWithoutEA{5};

  auto *Ir = IR::Create(Ctx);

  for (size_t I = 0; I < ModulesWithEA; ++I) {
    Module *M = Module::Create(Ctx);
    M->setPreferredEA(PreferredEA);
    EXPECT_NO_THROW(Ir->getModules().push_back(M));
  }

  for (size_t I = 0; I < ModulesWithoutEA; ++I) {
    Module *M = Module::Create(Ctx);
    EXPECT_NO_THROW(Ir->getModules().push_back(M));
  }

  const auto Modules = Ir->getModulesWithPreferredEA(PreferredEA);
  EXPECT_FALSE(Modules.empty());
  EXPECT_EQ(ModulesWithEA, Modules.size());
}

TEST(Unit_IR, getModulesContainingEA) {
  const gtirb::EA Ea{22678};
  const gtirb::EA EaOffset{2112};

  auto *Ir = IR::Create(Ctx);

  // EA at lower bound
  {
    Module *M = Module::Create(Ctx);
    M->getImageByteMap().setEAMinMax({Ea, Ea + EaOffset});
    EXPECT_NO_THROW(Ir->getModules().push_back(M));
  }

  // EA inside range
  {
    Module *M = Module::Create(Ctx);
    M->getImageByteMap().setEAMinMax({Ea - EaOffset, Ea + EaOffset});
    EXPECT_NO_THROW(Ir->getModules().push_back(M));
  }

  // EA at max (should not be returned)
  {
    Module *M = Module::Create(Ctx);
    M->getImageByteMap().setEAMinMax({Ea - EaOffset, Ea});
    EXPECT_NO_THROW(Ir->getModules().push_back(M));
  }

  const auto modules = Ir->getModulesContainingEA(Ea);
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
    M->getImageByteMap().setEAMinMax({EA(100), EA(200)});
    Original->getModules().push_back(M);
    Original->addTable("test", Table());

    MainID = Original->getModules().front()->getUUID();
    Original->toProtobuf(&Message);
    Original->setUUID(); // Avoid UUID conflict
  }
  IR *Result = IR::fromProtobuf(Ctx, Message);

  EXPECT_EQ(Result->getModules().front()->getUUID(), MainID);
  EXPECT_EQ(Result->getModulesContainingEA(EA(100)).size(), 1);
  EXPECT_EQ(Result->getTableSize(), 1);
  EXPECT_NE(Result->getTable("test"), nullptr);
}
