#include <gtirb/IR.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Module.hpp>
#include <proto/IR.pb.h>
#include <gtest/gtest.h>
#include <memory>

using namespace gtirb;

TEST(Unit_IR, ctor_0) { EXPECT_NO_THROW(gtirb::IR()); }

TEST(Unit_IR, getModulesWithPreferredEA) {
  const gtirb::EA preferredEA{22678};
  const size_t modulesWithEA{3};
  const size_t modulesWithoutEA{5};

  auto ir = gtirb::IR();

  for (size_t i = 0; i < modulesWithEA; ++i) {
    Module m;
    m.setPreferredEA(preferredEA);
    EXPECT_NO_THROW(ir.getModules().push_back(std::move(m)));
  }

  for (size_t i = 0; i < modulesWithoutEA; ++i) {
    Module m;
    EXPECT_NO_THROW(ir.getModules().push_back(std::move(m)));
  }

  const auto modules = ir.getModulesWithPreferredEA(preferredEA);
  EXPECT_FALSE(modules.empty());
  EXPECT_EQ(modulesWithEA, modules.size());
}

TEST(Unit_IR, getModulesContainingEA) {
  const gtirb::EA ea{22678};
  const gtirb::EA eaOffset{2112};

  auto ir = gtirb::IR();

  // EA at lower bound
  {
    Module m;
    m.getImageByteMap().setEAMinMax({ea, ea + eaOffset});
    EXPECT_NO_THROW(ir.getModules().push_back(std::move(m)));
  }

  // EA inside range
  {
    Module m;
    m.getImageByteMap().setEAMinMax({ea - eaOffset, ea + eaOffset});
    EXPECT_NO_THROW(ir.getModules().push_back(std::move(m)));
  }

  // EA at max (should not be returned)
  {
    Module m;
    m.getImageByteMap().setEAMinMax({ea - eaOffset, ea});
    EXPECT_NO_THROW(ir.getModules().push_back(std::move(m)));
  }

  const auto modules = ir.getModulesContainingEA(ea);
  EXPECT_FALSE(modules.empty());
  EXPECT_EQ(size_t(2), modules.size());
}

TEST(Unit_IR, protobufRoundTrip) {
  IR result;
  proto::IR message;
  UUID mainID;

  {
    IR original;
    Module m;
    m.getImageByteMap().setEAMinMax({EA(100), EA(200)});
    original.getModules().push_back(std::move(m));
    original.addTable("test", Table());

    mainID = original.getModules()[0].getUUID();
    original.toProtobuf(&message);
  }
  // original has been destroyed, so UUIDs can be reused
  result.fromProtobuf(message);

  EXPECT_EQ(result.getModules()[0].getUUID(), mainID);
  EXPECT_EQ(result.getModulesContainingEA(EA(100)).size(), 1);
  EXPECT_EQ(result.getTableSize(), 1);
  EXPECT_NE(result.getTable("test"), nullptr);
}
