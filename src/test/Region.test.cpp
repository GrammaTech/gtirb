#include <gtest/gtest.h>
#include <gtirb/Region.hpp>
#include <gtirb/RegionSet.hpp>
#include <memory>

TEST(Unit_Region, ctor_0)
{
    EXPECT_NO_THROW(gtirb::Region());
}

TEST(Unit_Region, addEA)
{
    auto region = std::make_unique<gtirb::Region>();
    EXPECT_NO_THROW(region->getEAs());

    auto eaSet = region->getEAs();
    EXPECT_TRUE(eaSet.empty());

    EXPECT_NO_THROW(region->addEA(gtirb::EA{2112}));
    EXPECT_NO_THROW(eaSet = region->getEAs());
    EXPECT_FALSE(eaSet.empty());
}
