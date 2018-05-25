#include <gtest/gtest.h>
#include <gtirb/Module.hpp>
#include <gtirb/RegionSet.hpp>
#include <memory>

TEST(Unit_RegionSet, ctor_0)
{
    EXPECT_NO_THROW(gtirb::RegionSet());
}

TEST(Unit_RegionSet, getRegion)
{
    gtirb::EA ea{22678};
    auto node = std::make_unique<gtirb::RegionSet>();
    EXPECT_NO_THROW(node->getRegion(ea));

    auto region = node->getRegion(ea);
    EXPECT_TRUE(region == nullptr);

    node->createRegion(ea);

    region = node->getRegion(ea);
    EXPECT_TRUE(region != nullptr);
}

TEST(Unit_RegionSet, getRegion_invalid)
{
    gtirb::EA ea{};
    auto node = std::make_unique<gtirb::RegionSet>();
    EXPECT_NO_THROW(node->getRegion(ea));
}
