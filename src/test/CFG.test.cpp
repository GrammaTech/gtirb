#include <gtest/gtest.h>
#include <gtirb/CFG.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <gtirb/RuntimeError.hpp>
#include <memory>

TEST(Unit_CFG, ctor_0)
{
    EXPECT_NO_THROW(gtirb::CFG());
}

TEST(Unit_CFG, validParent)
{
    auto parent = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::CFG>();
    EXPECT_TRUE(child->getIsValidParent(parent.get()));
    EXPECT_NO_THROW(parent->push_back(std::move(child)));
}

TEST(Unit_CFG, validParent_noException)
{
    auto parent = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::CFG>();
    EXPECT_TRUE(child->getIsValidParent(parent.get()));
    EXPECT_NO_THROW(parent->push_back(std::move(child)));
}

TEST(Unit_CFG, invalidParent)
{
    auto notAParent = std::make_unique<gtirb::Node>();
    auto child = std::make_unique<gtirb::CFG>();

    EXPECT_FALSE(child->getIsValidParent(notAParent.get()));
    EXPECT_THROW(notAParent->push_back(std::move(child)), gtirb::NodeStructureError);
}

TEST(Unit_CFG, alreadyAdded)
{
    auto parent = std::make_unique<gtirb::Module>();

    auto child = std::make_unique<gtirb::CFG>();
    EXPECT_TRUE(child->getIsValidParent(parent.get()));
    EXPECT_NO_THROW(parent->push_back(std::move(child)));

    auto childAgain = std::make_unique<gtirb::CFG>();
    EXPECT_FALSE(childAgain->getIsValidParent(parent.get()));
    EXPECT_THROW(parent->push_back(std::move(childAgain)), gtirb::NodeStructureError);
}
