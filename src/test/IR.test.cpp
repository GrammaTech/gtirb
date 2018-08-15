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

TEST(Unit_IR, ctor_0) { EXPECT_NO_THROW(gtirb::IR()); }

TEST(Unit_IR, getModulesWithPreferredEA) {
  const gtirb::EA PreferredEA{22678};
  const size_t ModulesWithEA{3};
  const size_t ModulesWithoutEA{5};

  auto Ir = gtirb::IR();

  for (size_t I = 0; I < ModulesWithEA; ++I) {
    Module M;
    M.setPreferredEA(PreferredEA);
    EXPECT_NO_THROW(Ir.getModules().push_back(std::move(M)));
  }

  for (size_t I = 0; I < ModulesWithoutEA; ++I) {
    Module M;
    EXPECT_NO_THROW(Ir.getModules().push_back(std::move(M)));
  }

  const auto Modules = Ir.getModulesWithPreferredEA(PreferredEA);
  EXPECT_FALSE(Modules.empty());
  EXPECT_EQ(ModulesWithEA, Modules.size());
}

TEST(Unit_IR, getModulesContainingEA) {
  const gtirb::EA Ea{22678};
  const gtirb::EA EaOffset{2112};

  auto Ir = gtirb::IR();

  // EA at lower bound
  {
    Module M;
    M.getImageByteMap().setEAMinMax({Ea, Ea + EaOffset});
    EXPECT_NO_THROW(Ir.getModules().push_back(std::move(M)));
  }

  // EA inside range
  {
    Module M;
    M.getImageByteMap().setEAMinMax({Ea - EaOffset, Ea + EaOffset});
    EXPECT_NO_THROW(Ir.getModules().push_back(std::move(M)));
  }

  // EA at max (should not be returned)
  {
    Module M;
    M.getImageByteMap().setEAMinMax({Ea - EaOffset, Ea});
    EXPECT_NO_THROW(Ir.getModules().push_back(std::move(M)));
  }

  const auto modules = Ir.getModulesContainingEA(Ea);
  EXPECT_FALSE(modules.empty());
  EXPECT_EQ(size_t(2), modules.size());
}

TEST(Unit_IR, addTable) {
  std::vector<int64_t> Table = {1, 2, 3};
  IR Ir;
  Ir.addTable("test", std::move(Table));

  EXPECT_NE(Ir.getTable("test"), nullptr);
  EXPECT_EQ(std::get<std::vector<int64_t>>(*Ir.getTable("test")),
            std::vector<int64_t>({1, 2, 3}));
}

TEST(Unit_IR, missingTable) {
  IR Ir;
  EXPECT_EQ(Ir.getTable("missing"), nullptr);
}

TEST(Unit_IR, protobufRoundTrip) {
  IR Result;
  proto::IR Message;
  UUID MainID;

  {
    IR Original;
    Module M;
    M.getImageByteMap().setEAMinMax({EA(100), EA(200)});
    Original.getModules().push_back(std::move(M));
    Original.addTable("test", Table());

    MainID = Original.getModules()[0].getUUID();
    Original.toProtobuf(&Message);
  }
  // original has been destroyed, so UUIDs can be reused
  Result.fromProtobuf(Message);

  EXPECT_EQ(Result.getModules()[0].getUUID(), MainID);
  EXPECT_EQ(Result.getModulesContainingEA(EA(100)).size(), 1);
  EXPECT_EQ(Result.getTableSize(), 1);
  EXPECT_NE(Result.getTable("test"), nullptr);
}
