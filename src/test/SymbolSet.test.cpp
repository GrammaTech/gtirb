#include <gtirb/Context.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolSet.hpp>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

TEST(Unit_SymbolSet, ctor_0) { EXPECT_NO_THROW(gtirb::SymbolSet()); }

TEST(Unit_SymbolSet, findSymbols) {
  Addr Ea1{22678};
  Addr Ea2{33678};
  gtirb::Module *M = Module::Create(Ctx);

  M->addSymbol(
      {Symbol::Create(Ctx, Ea1, "s1"), Symbol::Create(Ctx, Ea1, "s2")});

  Module::symbol_range SR = M->findSymbols(Ea1);
  EXPECT_EQ(std::distance(SR.begin(), SR.end()), 2);
  EXPECT_EQ(SR.begin()->getName(), "s1");
  EXPECT_EQ(std::next(SR.begin())->getName(), "s2");
  SR = M->findSymbols(Ea2);
  EXPECT_TRUE(SR.begin() == SR.end());

  M->addSymbol(Symbol::Create(Ctx, Ea2, "s3"));
  SR = M->findSymbols(Ea1);
  EXPECT_EQ(std::distance(SR.begin(), SR.end()), 2);
  EXPECT_EQ(SR.begin()->getName(), "s1");
  EXPECT_EQ(std::next(SR.begin())->getName(), "s2");
  SR = M->findSymbols(Ea2);
  EXPECT_EQ(std::distance(SR.begin(), SR.end()), 1);
  EXPECT_EQ(SR.begin()->getName(), "s3");
}

TEST(Unit_SymbolSet, getSymbolsInvalid) {
  Addr Ea{};
  gtirb::Module *M = Module::Create(Ctx);

  EXPECT_NO_THROW(M->findSymbols(Ea));
  auto R = M->findSymbols(Ea);
  EXPECT_EQ(std::distance(R.begin(), R.end()), 0);
}
