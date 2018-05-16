#include <gtest/gtest.h>
#include <gtirb/Module.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <gtirb/Symbol.hpp>
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

TEST(Unit_SymbolSet, getSymbols)
{
    gtirb::EA ea(22678);

    auto node = std::make_unique<gtirb::SymbolSet>();
    EXPECT_NO_THROW(node->getSymbols());
    EXPECT_TRUE(node->getSymbols().empty());
    EXPECT_TRUE(node->empty());
    EXPECT_EQ(size_t{0}, node->size());

    auto s1 = node->addSymbol(std::make_unique<gtirb::Symbol>(ea));
    auto s2 = node->addSymbol(std::make_unique<gtirb::Symbol>(ea));

    // Can store multiple symbols with the same EA
    EXPECT_NO_THROW(node->getSymbols());
    EXPECT_EQ(size_t{2}, node->getSymbols().size());
    EXPECT_FALSE(node->empty());
    EXPECT_EQ(size_t{2}, node->size());
    EXPECT_EQ(node->getSymbols(), (std::vector<gtirb::Symbol*>{s1, s2}));
}

TEST(Unit_SymbolSet, getSymbolsByEA)
{
    gtirb::EA ea1{22678};
    gtirb::EA ea2{33678};
    auto node = std::make_unique<gtirb::SymbolSet>();

    auto s1 = node->addSymbol(std::make_unique<gtirb::Symbol>(ea1));
    auto s2 = node->addSymbol(std::make_unique<gtirb::Symbol>(ea1));

    EXPECT_EQ(node->getSymbols(ea1), (std::vector<gtirb::Symbol*>{s1, s2}));
    EXPECT_TRUE(node->getSymbols(ea2).empty());

    // auto s3 = node->addSymbol(std::make_unique<gtirb::Symbol>(ea2));
    // EXPECT_EQ(node->getSymbols(ea1), (std::vector<gtirb::Symbol*>{s1, s2}));
    // EXPECT_EQ(node->getSymbols(ea2), (std::vector<gtirb::Symbol*>{s3}));
}

TEST(Unit_SymbolSet, getSymbolsInvalid)
{
    gtirb::EA ea{};
    auto node = std::make_unique<gtirb::SymbolSet>();
    EXPECT_NO_THROW(node->getSymbols(ea));
}
