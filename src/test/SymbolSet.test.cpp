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

TEST(Unit_SymbolSet, findSymbols)
{
    gtirb::EA ea1{22678};
    gtirb::EA ea2{33678};
    gtirb::SymbolSet symbols;

    symbols.emplace_back(ea1, "s1");
    symbols.emplace_back(ea2, "s2");

    EXPECT_EQ(findSymbols(symbols, ea1).size(), 2);
    EXPECT_EQ(findSymbols(symbols, ea1)[0]->getName(), "s1");
    EXPECT_EQ(findSymbols(symbols, ea1)[1]->getName(), "s2");
    EXPECT_TRUE(findSymbols(symbols, ea2).empty());

    symbols.emplace_back(ea2, "s3");
    EXPECT_EQ(findSymbols(symbols, ea1).size(), 2);
    EXPECT_EQ(findSymbols(symbols, ea1)[0]->getName(), "s1");
    EXPECT_EQ(findSymbols(symbols, ea1)[1]->getName(), "s2");
    EXPECT_EQ(findSymbols(symbols, ea2).size(), 1);
    EXPECT_EQ(findSymbols(symbols, ea2)[0]->getName(), "s3");
}

TEST(Unit_SymbolSet, getSymbolsInvalid)
{
    gtirb::EA ea{};
    gtirb::SymbolSet symbols;

    EXPECT_NO_THROW(findSymbols(symbols, ea));
    EXPECT_EQ(findSymbols(symbols, ea).size(), 0);
}
