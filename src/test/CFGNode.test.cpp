#include <gtest/gtest.h>
#include <gtirb/CFG.hpp>
#include <gtirb/CFGNode.hpp>
#include <gtirb/CFGNodeInfoCall.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <gtirb/RuntimeError.hpp>
#include <memory>

TEST(Unit_CFGNode, ctor_0)
{
    EXPECT_NO_THROW(gtirb::CFGNode());
}

TEST(Unit_CFGNode, validParent_cfg)
{
    auto parent = std::make_unique<gtirb::CFG>();
    auto child = std::make_unique<gtirb::CFGNode>();
    EXPECT_TRUE(child->getIsValidParent(parent.get()));
    EXPECT_NO_THROW(parent->push_back(std::move(child)));
}

TEST(Unit_CFGNode, validParent_cfgnode)
{
    auto parent = std::make_unique<gtirb::CFGNode>();
    auto child = std::make_unique<gtirb::CFGNode>();
    EXPECT_TRUE(child->getIsValidParent(parent.get()));
    EXPECT_NO_THROW(parent->push_back(std::move(child)));
}

TEST(Unit_CFGNode, invalidParent)
{
    auto notAParent = std::make_unique<gtirb::Node>();
    auto child = std::make_unique<gtirb::CFGNode>();

    EXPECT_FALSE(child->getIsValidParent(notAParent.get()));
    EXPECT_THROW(notAParent->push_back(std::move(child)), gtirb::NodeStructureError);
}

TEST(Unit_CFGNode, alreadyAdded)
{
    auto parent = std::make_unique<gtirb::CFG>();

    auto child = std::make_unique<gtirb::CFGNode>();
    EXPECT_TRUE(child->getIsValidParent(parent.get()));
    EXPECT_NO_THROW(parent->push_back(std::move(child)));

    // This should work just fine.
    auto childAgain = std::make_unique<gtirb::CFGNode>();
    EXPECT_TRUE(childAgain->getIsValidParent(parent.get()));
    EXPECT_NO_THROW(parent->push_back(std::move(childAgain)));
}

TEST(Unit_CFGNode, getCFGNodeInfo)
{
    auto node = std::make_unique<gtirb::CFGNode>();
    EXPECT_TRUE(node->getCFGNodeInfo() == nullptr);

    auto nodeInfo = std::make_unique<gtirb::CFGNodeInfoCall>();
    EXPECT_NO_THROW(node->push_back(std::move(nodeInfo)));

    EXPECT_TRUE(node->getCFGNodeInfo() != nullptr);
}
