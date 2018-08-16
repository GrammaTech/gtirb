#include <gtirb/Context.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolSet.hpp>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

TEST(Unit_SymbolSet, ctor_0) { EXPECT_NO_THROW(gtirb::SymbolSet()); }

TEST(Unit_SymbolSet, findSymbols) {
  gtirb::EA Ea1{22678};
  gtirb::EA Ea2{33678};
  gtirb::SymbolSet Symbols;

  addSymbol(Symbols, Symbol::Create(Ctx, Ea1, "s1"));
  addSymbol(Symbols, Symbol::Create(Ctx, Ea1, "s2"));

  EXPECT_EQ(findSymbols(Symbols, Ea1).size(), 2);
  EXPECT_EQ(findSymbols(Symbols, Ea1)[0]->getName(), "s1");
  EXPECT_EQ(findSymbols(Symbols, Ea1)[1]->getName(), "s2");
  EXPECT_TRUE(findSymbols(Symbols, Ea2).empty());

  addSymbol(Symbols, Symbol::Create(Ctx, Ea2, "s3"));
  EXPECT_EQ(findSymbols(Symbols, Ea1).size(), 2);
  EXPECT_EQ(findSymbols(Symbols, Ea1)[0]->getName(), "s1");
  EXPECT_EQ(findSymbols(Symbols, Ea1)[1]->getName(), "s2");
  EXPECT_EQ(findSymbols(Symbols, Ea2).size(), 1);
  EXPECT_EQ(findSymbols(Symbols, Ea2)[0]->getName(), "s3");
}

TEST(Unit_SymbolSet, getSymbolsInvalid) {
  gtirb::EA Ea{};
  gtirb::SymbolSet Symbols;

  EXPECT_NO_THROW(findSymbols(Symbols, Ea));
  EXPECT_EQ(findSymbols(Symbols, Ea).size(), 0);
}
