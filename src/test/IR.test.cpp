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

TEST(Unit_IR, getModulesWithPreferredAddr) {
  const Addr PreferredAddr{22678};
  const size_t ModulesWithAddr{3};
  const size_t ModulesWithoutAddr{5};

  auto Ir = gtirb::IR();

  for (size_t I = 0; I < ModulesWithAddr; ++I) {
    Module M;
    M.setPreferredAddr(PreferredAddr);
    EXPECT_NO_THROW(Ir.getModules().push_back(std::move(M)));
  }

  for (size_t I = 0; I < ModulesWithoutAddr; ++I) {
    Module M;
    M.setPreferredAddr(Addr{0});
    EXPECT_NO_THROW(Ir.getModules().push_back(std::move(M)));
  }

  const auto Modules = Ir.getModulesWithPreferredAddr(PreferredAddr);
  EXPECT_FALSE(Modules.empty());
  EXPECT_EQ(ModulesWithAddr, Modules.size());
}

TEST(Unit_IR, getModulesContainingAddr) {
  const Addr Ea{22678};
  const uint64_t EaOffset{2112};

  auto Ir = gtirb::IR();

  // Addr at lower bound
  {
    Module M;
    M.getImageByteMap().setAddrMinMax({Ea, Ea + EaOffset});
    EXPECT_NO_THROW(Ir.getModules().push_back(std::move(M)));
  }

  // Addr inside range
  {
    Module M;
    M.getImageByteMap().setAddrMinMax({Ea - EaOffset, Ea + EaOffset});
    EXPECT_NO_THROW(Ir.getModules().push_back(std::move(M)));
  }

  // Addr at max (should not be returned)
  {
    Module M;
    M.getImageByteMap().setAddrMinMax({Ea - EaOffset, Ea});
    EXPECT_NO_THROW(Ir.getModules().push_back(std::move(M)));
  }

  const auto modules = Ir.getModulesContainingAddr(Ea);
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
    M.getImageByteMap().setAddrMinMax({Addr(100), Addr(200)});
    Original.getModules().push_back(std::move(M));
    Original.addTable("test", Table());

    MainID = Original.getModules()[0].getUUID();
    Original.toProtobuf(&Message);
  }
  // original has been destroyed, so UUIDs can be reused
  Result.fromProtobuf(Message);

  EXPECT_EQ(Result.getModules()[0].getUUID(), MainID);
  EXPECT_EQ(Result.getModulesContainingAddr(Addr(100)).size(), 1);
  EXPECT_EQ(Result.getTableSize(), 1);
  EXPECT_NE(Result.getTable("test"), nullptr);
}
