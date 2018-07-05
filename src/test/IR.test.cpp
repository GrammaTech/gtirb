#include <gtest/gtest.h>
#include <proto/IR.pb.h>
#include <gtirb/IR.hpp>
#include <gtirb/Module.hpp>
#include <memory>

using namespace gtirb;

TEST(Unit_IR, ctor_0)
{
    EXPECT_NO_THROW(gtirb::IR());
}

TEST(Unit_IR, getMainModule)
{
    // Main module is created in the constructor
    auto ir = gtirb::IR();
    EXPECT_NO_THROW(ir.getMainModule());
}

TEST(Unit_IR, getModulesWithPreferredEA)
{
    const gtirb::EA preferredEA{22678};
    const size_t modulesWithEA{3};
    const size_t modulesWithoutEA{5};

    auto ir = gtirb::IR();

    for(size_t i = 0; i < modulesWithEA; ++i)
    {
        auto m = std::make_unique<gtirb::Module>();
        m->setPreferredEA(preferredEA);
        EXPECT_NO_THROW(ir.addModule(std::move(m)));
    }

    for(size_t i = 0; i < modulesWithoutEA; ++i)
    {
        auto m = std::make_unique<gtirb::Module>();
        EXPECT_NO_THROW(ir.addModule(std::move(m)));
    }

    const auto modules = ir.getModulesWithPreferredEA(preferredEA);
    EXPECT_FALSE(modules.empty());
    EXPECT_EQ(modulesWithEA, modules.size());
}

TEST(Unit_IR, getModulesContainingEA)
{
    const gtirb::EA ea{22678};
    const gtirb::EA eaOffset{2112};

    auto ir = gtirb::IR();

    // EA at lower bound
    {
        auto m = std::make_unique<gtirb::Module>();
        m->setEAMinMax({ea, ea + eaOffset});
        EXPECT_NO_THROW(ir.addModule(std::move(m)));
    }

    // EA inside range
    {
        auto m = std::make_unique<gtirb::Module>();
        m->setEAMinMax({ea - eaOffset, ea + eaOffset});
        EXPECT_NO_THROW(ir.addModule(std::move(m)));
    }

    // EA at max (should not be returned)
    {
        auto m = std::make_unique<gtirb::Module>();
        m->setEAMinMax({ea - eaOffset, ea});
        EXPECT_NO_THROW(ir.addModule(std::move(m)));
    }

    const auto modules = ir.getModulesContainingEA(ea);
    EXPECT_FALSE(modules.empty());
    EXPECT_EQ(size_t(2), modules.size());
}

TEST(Unit_IR, protobufRoundTrip)
{
    IR original;
    auto m = std::make_unique<Module>();
    m->setEAMinMax({EA(100), EA(200)});
    original.addModule(std::move(m));
    original.addTable("test", std::make_unique<Table>());

    IR result;
    proto::IR message;
    original.toProtobuf(&message);
    result.fromProtobuf(message);

    EXPECT_EQ(result.getMainModule().getUUID(), original.getMainModule().getUUID());
    EXPECT_EQ(result.getModulesContainingEA(EA(100)).size(), 1);
    EXPECT_EQ(result.getTableSize(), 1);
    EXPECT_NE(result.getTable("test"), nullptr);
}
