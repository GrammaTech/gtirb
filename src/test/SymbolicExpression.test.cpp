#include <gtirb/Context.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <proto/SymbolicExpression.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

TEST(Unit_SymbolicExpression, protobufRoundTrip) {
  Symbol* Sym1 = Symbol::Create(Ctx, Addr(1), "test1");
  Symbol* Sym2 = Symbol::Create(Ctx, Addr(2), "test2");

  // SymStackConst
  {
    SymbolicExpression original(SymStackConst{true, 1, 2, {Sym1}});

    gtirb::SymbolicExpression Result;
    auto Message = toProtobuf(original);
    fromProtobuf(Ctx, Result, Message);

    SymStackConst S;
    EXPECT_NO_THROW(S = std::get<SymStackConst>(Result));
    EXPECT_EQ(S.Negate, true);
    EXPECT_EQ(S.Offset, 1);
    EXPECT_EQ(S.Displacement, 2);
    EXPECT_EQ(S.Sym.get(Ctx)->getName(), "test1");
  }

  // SymAddrConst
  {
    SymbolicExpression original(SymAddrConst{1, {Sym1}});

    gtirb::SymbolicExpression Result;
    auto Message = toProtobuf(original);
    fromProtobuf(Ctx, Result, Message);

    SymAddrConst S;
    EXPECT_NO_THROW(S = std::get<SymAddrConst>(Result));
    EXPECT_EQ(S.Displacement, 1);
    EXPECT_EQ(S.Sym.get(Ctx)->getName(), "test1");
  }

  // SymAddrAddr
  {
    SymbolicExpression original(SymAddrAddr{1, 2, {Sym1}, {Sym2}});

    gtirb::SymbolicExpression Result;
    auto Message = toProtobuf(original);
    fromProtobuf(Ctx, Result, Message);

    SymAddrAddr S;
    EXPECT_NO_THROW(S = std::get<SymAddrAddr>(Result));
    EXPECT_EQ(S.Scale, 1);
    EXPECT_EQ(S.Offset, 2);
    EXPECT_EQ(S.Sym1.get(Ctx)->getName(), "test1");
    EXPECT_EQ(S.Sym2.get(Ctx)->getName(), "test2");
  }
}
