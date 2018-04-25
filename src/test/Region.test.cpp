#include <gtest/gtest.h>
#include <gtirb/NodeStructureError.hpp>
#include <gtirb/Region.hpp>
#include <gtirb/RegionSet.hpp>
#include <memory>

TEST(Unit_Region, ctor_0)
{
    EXPECT_NO_THROW(gtirb::Region());
}

TEST(Unit_Region, validParent)
{
    auto module = std::make_unique<gtirb::RegionSet>();
    auto child = std::make_unique<gtirb::Region>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TEST(Unit_Region, validParent_noException)
{
    auto module = std::make_unique<gtirb::RegionSet>();
    auto child = std::make_unique<gtirb::Region>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TEST(Unit_Region, invalidParent)
{
    auto notAParent = std::make_unique<gtirb::Node>();
    auto child = std::make_unique<gtirb::Region>();

    EXPECT_FALSE(child->getIsValidParent(notAParent.get()));
    EXPECT_THROW(notAParent->push_back(std::move(child)), gtirb::NodeStructureError);
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
