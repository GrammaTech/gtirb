#include <gtest/gtest.h>
#include <gtirb/CFGNode.hpp>
#include <gtirb/CFGNodeInfo.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <gtirb/RuntimeError.hpp>
#include <gtirb/Symbol.hpp>
#include <memory>

TEST(Unit_CFGNodeInfo, ctor_0)
{
    EXPECT_NO_THROW(gtirb::CFGNodeInfo());
}

TEST(Unit_CFGNodeInfo, validParent)
{
    auto parent = std::make_unique<gtirb::CFGNode>();
    auto child = std::make_unique<gtirb::CFGNodeInfo>();
    EXPECT_TRUE(child->getIsValidParent(parent.get()));
    EXPECT_NO_THROW(parent->push_back(std::move(child)));
}

TEST(Unit_CFGNodeInfo, validParent_noException)
{
    auto parent = std::make_unique<gtirb::CFGNode>();
    auto child = std::make_unique<gtirb::CFGNodeInfo>();
    EXPECT_TRUE(child->getIsValidParent(parent.get()));
    EXPECT_NO_THROW(parent->push_back(std::move(child)));
}

TEST(Unit_CFGNodeInfo, invalidParent)
{
    auto notAParent = std::make_unique<gtirb::Node>();
    auto child = std::make_unique<gtirb::CFGNodeInfo>();

    EXPECT_FALSE(child->getIsValidParent(notAParent.get()));
    EXPECT_THROW(notAParent->push_back(std::move(child)), gtirb::NodeStructureError);
}

TEST(Unit_CFGNodeInfo, alreadyAdded)
{
    auto parent = std::make_unique<gtirb::CFGNode>();

    auto child = std::make_unique<gtirb::CFGNodeInfo>();
    EXPECT_TRUE(child->getIsValidParent(parent.get()));
    EXPECT_NO_THROW(parent->push_back(std::move(child)));

    auto childAgain = std::make_unique<gtirb::CFGNodeInfo>();
    EXPECT_FALSE(childAgain->getIsValidParent(parent.get()));
    EXPECT_THROW(parent->push_back(std::move(childAgain)), gtirb::NodeStructureError);
}

TEST(Unit_CFGNodeInfo, setProcedureNameSymbol)
{
    auto symbol = std::make_shared<gtirb::Symbol>();
    auto node = std::make_unique<gtirb::CFGNodeInfo>();

    EXPECT_NO_THROW(node->setProcedureNameSymbol(symbol.get()));
}

TEST(Unit_CFGNodeInfo, setProcedureNameSymbol_exception)
{
    auto symbol = std::make_unique<gtirb::Symbol>();
    auto node = std::make_unique<gtirb::CFGNodeInfo>();

    EXPECT_THROW(node->setProcedureNameSymbol(symbol.get()), std::bad_weak_ptr);
}
