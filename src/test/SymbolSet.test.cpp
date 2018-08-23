#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolSet.hpp>
#include <gtest/gtest.h>

using namespace gtirb;

TEST(Unit_SymbolSet, ctor_0) { EXPECT_NO_THROW(gtirb::SymbolSet()); }

TEST(Unit_SymbolSet, findSymbols) {
  Addr Ea1{22678};
  Addr Ea2{33678};
  gtirb::SymbolSet Symbols;

  addSymbol(Symbols, Symbol(Ea1, "s1"));
  addSymbol(Symbols, Symbol(Ea1, "s2"));

  EXPECT_EQ(findSymbols(Symbols, Ea1).size(), 2);
  EXPECT_EQ(findSymbols(Symbols, Ea1)[0]->getName(), "s1");
  EXPECT_EQ(findSymbols(Symbols, Ea1)[1]->getName(), "s2");
  EXPECT_TRUE(findSymbols(Symbols, Ea2).empty());

  addSymbol(Symbols, Symbol(Ea2, "s3"));
  EXPECT_EQ(findSymbols(Symbols, Ea1).size(), 2);
  EXPECT_EQ(findSymbols(Symbols, Ea1)[0]->getName(), "s1");
  EXPECT_EQ(findSymbols(Symbols, Ea1)[1]->getName(), "s2");
  EXPECT_EQ(findSymbols(Symbols, Ea2).size(), 1);
  EXPECT_EQ(findSymbols(Symbols, Ea2)[0]->getName(), "s3");
}

TEST(Unit_SymbolSet, getSymbolsInvalid) {
  Addr Ea{12};
  gtirb::SymbolSet Symbols;

  EXPECT_NO_THROW(findSymbols(Symbols, Ea));
  EXPECT_EQ(findSymbols(Symbols, Ea).size(), 0);
}
