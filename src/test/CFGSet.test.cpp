#include <gtest/gtest.h>
#include <gtirb/Module.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <gtirb/CFGSet.hpp>
#include <memory>

TEST(Unit_CFGSet, ctor_0)
{
    EXPECT_NO_THROW(gtirb::CFGSet());
}

TEST(Unit_CFGSet, validParent)
{
    auto parent = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::CFGSet>();
    EXPECT_TRUE(child->getIsValidParent(parent.get()));
    EXPECT_NO_THROW(parent->push_back(std::move(child)));
}

TEST(Unit_CFGSet, invalidParent)
{
    auto notAParent = std::make_unique<gtirb::Node>();
    auto child = std::make_unique<gtirb::CFGSet>();

    EXPECT_FALSE(child->getIsValidParent(notAParent.get()));
    EXPECT_THROW(notAParent->push_back(std::move(child)), gtirb::NodeStructureError);
}

TEST(Unit_CFGSet, noSiblings)
{
    auto parent = std::make_unique<gtirb::Module>();

    // Only the first child is valid.
    {
        auto child = std::make_unique<gtirb::CFGSet>();
        EXPECT_TRUE(child->getIsValidParent(parent.get()));
        EXPECT_NO_THROW(parent->push_back(std::move(child)));
    }

    {
        auto child = std::make_unique<gtirb::CFGSet>();
        EXPECT_FALSE(child->getIsValidParent(parent.get()));
        EXPECT_THROW(parent->push_back(std::move(child)), gtirb::NodeStructureError);
    }
}

TEST(Unit_CFGSet, getCFG_EA)
{
    gtirb::EA ea{22678};
    auto node = std::make_unique<gtirb::CFGSet>();
    EXPECT_NO_THROW(node->getCFG(ea));
    EXPECT_TRUE(node->empty());
    EXPECT_EQ(size_t{0}, node->size());

    auto child = node->getCFG(ea);
    EXPECT_TRUE(child == nullptr);
    EXPECT_TRUE(node->empty());
    EXPECT_EQ(size_t{0}, node->size());

    child = node->getOrCreateCFG(ea);
    EXPECT_TRUE(child != nullptr);
    EXPECT_FALSE(node->empty());
    EXPECT_EQ(size_t{1}, node->size());

    child = node->getCFG(ea);
    EXPECT_TRUE(child != nullptr);
    EXPECT_FALSE(node->empty());
    EXPECT_EQ(size_t{1}, node->size());

    // Make sure we don't create it again.
    child = node->getOrCreateCFG(ea);
    EXPECT_TRUE(child != nullptr);
    EXPECT_FALSE(node->empty());
    EXPECT_EQ(size_t{1}, node->size());
}

TEST(Unit_CFGSet, getCFG_ProcedureName)
{
    const gtirb::EA ea{22678};
    const std::string procedureName{"Foo"};

    auto node = std::make_unique<gtirb::CFGSet>();
    EXPECT_NO_THROW(node->getCFG(ea));
    EXPECT_TRUE(node->empty());
    EXPECT_EQ(size_t{0}, node->size());

    child = node->getOrCreateCFG(ea);
    EXPECT_TRUE(child != nullptr);
    EXPECT_FALSE(node->empty());
    EXPECT_EQ(size_t{1}, node->size());
    EXPECT_NO_THROW(child->setProcedureName(procedureName));

    child = node->getCFG(procedureName);
    EXPECT_TRUE(child != nullptr);
    EXPECT_FALSE(node->empty());
    EXPECT_EQ(size_t{1}, node->size());
}

TEST(Unit_CFGSet, getCFG_invalid)
{
    gtirb::EA ea{};
    auto node = std::make_unique<gtirb::CFGSet>();
    EXPECT_NO_THROW(node->getCFG(ea));
}
