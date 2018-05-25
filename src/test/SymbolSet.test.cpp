#include <gtest/gtest.h>
#include <gtirb/Module.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolSet.hpp>
#include <memory>

using namespace gtirb;

TEST(Unit_SymbolSet, ctor_0)
{
    EXPECT_NO_THROW(gtirb::SymbolSet());
}

TEST(Unit_SymbolSet, getSymbols)
{
    gtirb::EA ea(22678);

    auto node = std::make_unique<gtirb::SymbolSet>();
    EXPECT_NO_THROW(node->getSymbols());
    EXPECT_TRUE(node->getSymbols().empty());

    auto s1 = node->addSymbol(Symbol(ea));
    auto s2 = node->addSymbol(Symbol(ea));

    // Can store multiple symbols with the same EA
    EXPECT_NO_THROW(node->getSymbols());
    EXPECT_EQ(size_t{2}, node->getSymbols().size());
    EXPECT_EQ(node->getSymbols()[0].getEA(), ea);
    EXPECT_EQ(node->getSymbols()[1].getEA(), ea);
}

TEST(Unit_SymbolSet, getSymbolsByEA)
{
    gtirb::EA ea1{22678};
    gtirb::EA ea2{33678};
    auto node = std::make_unique<gtirb::SymbolSet>();

    auto& s1 = node->addSymbol(Symbol(ea1));
    s1.setName("s1");
    auto& s2 = node->addSymbol(Symbol(ea1));
    s2.setName("s2");

    EXPECT_EQ(node->getSymbols(ea1).size(), 2);
    EXPECT_EQ(node->getSymbols(ea1)[0]->getName(), "s1");
    EXPECT_EQ(node->getSymbols(ea1)[1]->getName(), "s2");
    EXPECT_TRUE(node->getSymbols(ea2).empty());

    auto& s3 = node->addSymbol(Symbol(ea2));
    s3.setName("s3");
    EXPECT_EQ(node->getSymbols(ea1).size(), 2);
    EXPECT_EQ(node->getSymbols(ea1)[0]->getName(), "s1");
    EXPECT_EQ(node->getSymbols(ea1)[1]->getName(), "s2");
    EXPECT_EQ(node->getSymbols(ea2).size(), 1);
    EXPECT_EQ(node->getSymbols(ea2)[0]->getName(), "s3");
}

TEST(Unit_SymbolSet, getSymbolsInvalid)
{
    gtirb::EA ea{};
    auto node = std::make_unique<gtirb::SymbolSet>();
    EXPECT_NO_THROW(node->getSymbols(ea));
}
