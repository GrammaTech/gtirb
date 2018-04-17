#include <gtest/gtest.h>
#include <gtirb/Module.hpp>
#include <gtirb/AddrRanges.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <gtirb/RuntimeError.hpp>
#include <memory>

TEST(Unit_AddrRanges, ctor_0)
{
    EXPECT_NO_THROW(gtirb::AddrRanges());
}

TEST(Unit_AddrRanges, validParent)
{
    auto module = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::AddrRanges>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TEST(Unit_AddrRanges, validParent_noException)
{
    auto module = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::AddrRanges>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TEST(Unit_AddrRanges, invalidParent)
{
    auto notAModule = std::make_unique<gtirb::Node>();
    auto child = std::make_unique<gtirb::AddrRanges>();

    EXPECT_FALSE(child->getIsValidParent(notAModule.get()));
    EXPECT_THROW(notAModule->push_back(std::move(child)), gtirb::NodeStructureError);
}

TEST(Unit_AddrRanges, alreadyAdded)
{
    auto module = std::make_unique<gtirb::Module>();

    auto child = std::make_unique<gtirb::AddrRanges>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));

    auto childAgain = std::make_unique<gtirb::AddrRanges>();
    EXPECT_FALSE(childAgain->getIsValidParent(module.get()));
    EXPECT_THROW(module->push_back(std::move(childAgain)), gtirb::NodeStructureError);
}

TEST(Unit_AddrRanges, validRange)
{
    auto node = std::make_unique<gtirb::AddrRanges>();

    EXPECT_TRUE(node->getRangeVector().empty());
    EXPECT_NO_THROW(node->addRange({gtirb::EA{1221}, gtirb::EA{2112}}));
    EXPECT_FALSE(node->getRangeVector().empty());
    EXPECT_EQ(size_t{1}, node->getRangeVector().size());
}

TEST(Unit_AddrRanges, invalidRange)
{
    auto node = std::make_unique<gtirb::AddrRanges>();

    EXPECT_TRUE(node->getRangeVector().empty());
    EXPECT_THROW(node->addRange({gtirb::EA{2112}, gtirb::EA{1221}}), gtirb::RuntimeError);
    EXPECT_TRUE(node->getRangeVector().empty());
    EXPECT_EQ(size_t{0}, node->getRangeVector().size());
}
