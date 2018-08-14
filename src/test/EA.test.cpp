#include <gtirb/EA.hpp>
#include <gtest/gtest.h>
#include <sstream>

using namespace gtirb;

TEST(Unit_EA, ctor_0) {
  EXPECT_NO_THROW(EA());
  EXPECT_EQ(EA(BadAddress), EA());
}

TEST(Unit_EA, ctor_1) {
  EXPECT_NO_THROW(EA(2112));

  auto Ea = EA(2112);
  EXPECT_EQ(uint64_t(2112), Ea.get());
}

TEST(Unit_EA, fromIntegers) {
  uint64_t U64 = 1;
  EA U64EA(U64);
  EXPECT_EQ(U64EA.get(), U64);

  int64_t S64 = 2;
  EA S64EA(S64);
  EXPECT_EQ(S64EA.get(), S64);

  uint32_t U32 = 3;
  EA U32EA(U32);
  EXPECT_EQ(U32EA.get(), U32);

  int32_t S32 = 4;
  EA S32EA(S32);
  EXPECT_EQ(S32EA.get(), S32);

  uint16_t U16 = 5;
  EA U16EA(U16);
  EXPECT_EQ(U16EA.get(), U16);

  int16_t S16 = 6;
  EA S16EA(S16);
  EXPECT_EQ(S16EA.get(), S16);

  uint8_t U8 = 7;
  EA U8EA(U8);
  EXPECT_EQ(U8EA.get(), U8);

  int8_t S8 = 8;
  EA S8EA(S8);
  EXPECT_EQ(S8EA.get(), S8);
}

TEST(Unit_EA, toIntegers) {
  EA Ea(1);
  EXPECT_EQ(static_cast<uint64_t>(Ea), static_cast<uint64_t>(1));
  EXPECT_EQ(static_cast<int64_t>(Ea), static_cast<int64_t>(1));

  EXPECT_EQ(static_cast<uint32_t>(Ea), static_cast<uint32_t>(1));
  EXPECT_EQ(static_cast<int32_t>(Ea), static_cast<int32_t>(1));

  EXPECT_EQ(static_cast<uint16_t>(Ea), static_cast<uint16_t>(1));
  EXPECT_EQ(static_cast<int16_t>(Ea), static_cast<int16_t>(1));

  EXPECT_EQ(static_cast<uint8_t>(Ea), static_cast<uint8_t>(1));
  EXPECT_EQ(static_cast<int8_t>(Ea), static_cast<int8_t>(1));
}

TEST(Unit_EA, comparison) {
  auto Ea1 = EA(2112);
  auto Ea2 = EA(1221);

  EXPECT_GT(Ea1, Ea2);
  EXPECT_TRUE(Ea1 > Ea2);

  EXPECT_NE(Ea1, Ea2);
  EXPECT_TRUE(Ea1 != Ea2);

  EXPECT_LT(Ea2, Ea1);
  EXPECT_TRUE(Ea2 < Ea1);

  EXPECT_FALSE(Ea1 == Ea2);
}

TEST(Unit_EA, arithmetic) {
  EA Ea(10);

  EXPECT_EQ(Ea + EA(5), EA(15));
  EXPECT_EQ(Ea + uint64_t(5), EA(15));
  EXPECT_EQ(Ea - EA(5), EA(5));
  EXPECT_EQ(Ea - uint64_t(5), EA(5));

  EA Ea2(10);
  EXPECT_EQ(Ea2 += EA(5), EA(15));
  EXPECT_EQ(Ea2, EA(15));

  EA Ea3(10);
  EXPECT_EQ(Ea3 += uint64_t(5), EA(15));
  EXPECT_EQ(Ea3, EA(15));

  EA Ea4(10);
  EXPECT_EQ(Ea4 -= EA(5), EA(5));
  EXPECT_EQ(Ea4, EA(5));

  EA Ea5(10);
  EXPECT_EQ(Ea5 -= uint64_t(5), EA(5));
  EXPECT_EQ(Ea5, EA(5));
}

TEST(Unit_EA, set) {
  auto Ea1 = EA(2112);
  auto Ea2 = EA();

  Ea2.set(uint64_t(2112));

  EXPECT_EQ(Ea1, Ea2);
  EXPECT_EQ(uint64_t(2112), Ea2.get());
}

TEST(Unit_EA, ostream) {
  std::ostringstream Os;

  Os << 123 << " " << EA(456) << " 789";
  EXPECT_EQ(Os.str(), "123 0x1c8 789");
}
