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

    region = node->getOrCreateRegion(ea);
    EXPECT_TRUE(region != nullptr);

    region = node->getRegion(ea);
    EXPECT_TRUE(region != nullptr);

    // Make sure we don't create it again.
    auto region2 = node->getOrCreateRegion(ea);
    EXPECT_TRUE(region2 != nullptr);
    EXPECT_EQ(region, region2);
}

TEST(Unit_RegionSet, getRegion_invalid)
{
    gtirb::EA ea{};
    auto node = std::make_unique<gtirb::RegionSet>();
    EXPECT_NO_THROW(node->getRegion(ea));
}
