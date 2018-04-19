#include <gtest/gtest.h>
#include <gtirb/Module.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <memory>

TEST(Unit_ImageByteMap, ctor_0)
{
    EXPECT_NO_THROW(gtirb::ImageByteMap());
}

TEST(Unit_ImageByteMap, validParent)
{
    auto module = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::ImageByteMap>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TEST(Unit_ImageByteMap, validParent_noException)
{
    auto module = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::ImageByteMap>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TEST(Unit_ImageByteMap, invalidParent)
{
    auto notAParent = std::make_unique<gtirb::Node>();
    auto child = std::make_unique<gtirb::ImageByteMap>();

    EXPECT_FALSE(child->getIsValidParent(notAParent.get()));
    EXPECT_THROW(notAParent->push_back(std::move(child)), gtirb::NodeStructureError);
}
