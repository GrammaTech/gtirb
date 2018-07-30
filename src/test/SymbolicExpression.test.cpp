#include <gtirb/SymbolicExpression.hpp>
#include <proto/SymbolicExpression.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

TEST(Unit_SymbolicExpression, protobufRoundTripStackConst) {
  Symbol sym1(EA(1), "test1");
  Symbol sym2(EA(2), "test2");

  // SymStackConst
  {
    SymbolicExpression original(SymStackConst{true, 1, 2, {sym1}});

    gtirb::SymbolicExpression result;
    auto message = toProtobuf(original);
    fromProtobuf(result, message);

    SymStackConst s;
    EXPECT_NO_THROW(s = boost::get<SymStackConst>(result));
    EXPECT_EQ(s.negate, true);
    EXPECT_EQ(s.offset, 1);
    EXPECT_EQ(s.displacement, 2);
    EXPECT_EQ(s.symbol->getName(), "test1");
  }

  // SymAddrConst
  {
    SymbolicExpression original(SymAddrConst{1, {sym1}});

    gtirb::SymbolicExpression result;
    auto message = toProtobuf(original);
    fromProtobuf(result, message);

    SymAddrConst s;
    EXPECT_NO_THROW(s = boost::get<SymAddrConst>(result));
    EXPECT_EQ(s.displacement, 1);
    EXPECT_EQ(s.symbol->getName(), "test1");
  }

  // SymAddrAddr
  {
    SymbolicExpression original(SymAddrAddr{1, 2, {sym1}, {sym2}});

    gtirb::SymbolicExpression result;
    auto message = toProtobuf(original);
    fromProtobuf(result, message);

    SymAddrAddr s;
    EXPECT_NO_THROW(s = boost::get<SymAddrAddr>(result));
    EXPECT_EQ(s.scale, 1);
    EXPECT_EQ(s.offset, 2);
    EXPECT_EQ(s.symbol1->getName(), "test1");
    EXPECT_EQ(s.symbol2->getName(), "test2");
  }
}
