#include <gtest/gtest.h>
#include <gtirb/Module.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <gtirb/RegionSet.hpp>
#include <memory>

TEST(Unit_RegionSet, ctor_0)
{
    EXPECT_NO_THROW(gtirb::RegionSet());
}

TEST(Unit_RegionSet, validParent)
{
    auto parent = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::RegionSet>();
    EXPECT_TRUE(child->getIsValidParent(parent.get()));
    EXPECT_NO_THROW(parent->push_back(std::move(child)));
}

TEST(Unit_RegionSet, invalidParent)
{
    auto notAParent = std::make_unique<gtirb::Node>();
    auto child = std::make_unique<gtirb::RegionSet>();

    EXPECT_FALSE(child->getIsValidParent(notAParent.get()));
    EXPECT_THROW(notAParent->push_back(std::move(child)), gtirb::NodeStructureError);
}

TEST(Unit_RegionSet, noSiblings)
{
    auto parent = std::make_unique<gtirb::Module>();

    // Only the first child is valid.
    {
        auto child = std::make_unique<gtirb::RegionSet>();
        EXPECT_TRUE(child->getIsValidParent(parent.get()));
        EXPECT_NO_THROW(parent->push_back(std::move(child)));
    }

    {
        auto child = std::make_unique<gtirb::RegionSet>();
        EXPECT_FALSE(child->getIsValidParent(parent.get()));
        EXPECT_THROW(parent->push_back(std::move(child)), gtirb::NodeStructureError);
    }
}

TEST(Unit_RegionSet, getRegion)
{
    gtirb::EA ea{22678};
    auto node = std::make_unique<gtirb::RegionSet>();
    EXPECT_NO_THROW(node->getRegion(ea));
    EXPECT_TRUE(node->empty());
    EXPECT_EQ(size_t{0}, node->size());

    auto region = node->getRegion(ea);
    EXPECT_TRUE(region == nullptr);
    EXPECT_TRUE(node->empty());
    EXPECT_EQ(size_t{0}, node->size());

    region = node->getOrCreateRegion(ea);
    EXPECT_TRUE(region != nullptr);
    EXPECT_FALSE(node->empty());
    EXPECT_EQ(size_t{1}, node->size());

    region = node->getRegion(ea);
    EXPECT_TRUE(region != nullptr);
    EXPECT_FALSE(node->empty());
    EXPECT_EQ(size_t{1}, node->size());

    // Make sure we don't create it again.
    region = node->getOrCreateRegion(ea);
    EXPECT_TRUE(region != nullptr);
    EXPECT_FALSE(node->empty());
    EXPECT_EQ(size_t{1}, node->size());
}

TEST(Unit_RegionSet, getRegion_invalid)
{
    gtirb::EA ea{};
    auto node = std::make_unique<gtirb::RegionSet>();
    EXPECT_NO_THROW(node->getRegion(ea));
}
