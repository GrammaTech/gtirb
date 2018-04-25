#include <gtest/gtest.h>
#include <gtirb/Module.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <gtirb/SymbolSet.hpp>
#include <memory>

TEST(Unit_SymbolSet, ctor_0)
{
    EXPECT_NO_THROW(gtirb::SymbolSet());
}

TEST(Unit_SymbolSet, validParent)
{
    auto parent = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::SymbolSet>();
    EXPECT_TRUE(child->getIsValidParent(parent.get()));
    EXPECT_NO_THROW(parent->push_back(std::move(child)));
}

TEST(Unit_SymbolSet, invalidParent)
{
    auto notAParent = std::make_unique<gtirb::Node>();
    auto child = std::make_unique<gtirb::SymbolSet>();

    EXPECT_FALSE(child->getIsValidParent(notAParent.get()));
    EXPECT_THROW(notAParent->push_back(std::move(child)), gtirb::NodeStructureError);
}

TEST(Unit_SymbolSet, noSiblings)
{
    auto parent = std::make_unique<gtirb::Module>();

    // Only the first child is valid.
    {
        auto child = std::make_unique<gtirb::SymbolSet>();
        EXPECT_TRUE(child->getIsValidParent(parent.get()));
        EXPECT_NO_THROW(parent->push_back(std::move(child)));
    }

    {
        auto child = std::make_unique<gtirb::SymbolSet>();
        EXPECT_FALSE(child->getIsValidParent(parent.get()));
        EXPECT_THROW(parent->push_back(std::move(child)), gtirb::NodeStructureError);
    }
}

TEST(Unit_SymbolSet, getSymbol)
{
    gtirb::EA ea{22678};
    auto node = std::make_unique<gtirb::SymbolSet>();
    EXPECT_NO_THROW(node->getSymbol(ea));
    EXPECT_TRUE(node->empty());
    EXPECT_EQ(size_t{0}, node->size());

    auto symbol = node->getSymbol(ea);
    EXPECT_TRUE(symbol == nullptr);
    EXPECT_TRUE(node->empty());
    EXPECT_EQ(size_t{0}, node->size());

    symbol = node->getOrCreateSymbol(ea);
    EXPECT_TRUE(symbol != nullptr);
    EXPECT_FALSE(node->empty());
    EXPECT_EQ(size_t{1}, node->size());

    symbol = node->getSymbol(ea);
    EXPECT_TRUE(symbol != nullptr);
    EXPECT_FALSE(node->empty());
    EXPECT_EQ(size_t{1}, node->size());

    // Make sure we don't create it again.
    symbol = node->getOrCreateSymbol(ea);
    EXPECT_TRUE(symbol != nullptr);
    EXPECT_FALSE(node->empty());
    EXPECT_EQ(size_t{1}, node->size());
}

TEST(Unit_SymbolSet, getSymbol_invalid)
{
    gtirb::EA ea{};
    auto node = std::make_unique<gtirb::SymbolSet>();
    EXPECT_NO_THROW(node->getSymbol(ea));
}
