#include <gtirb/Addr.hpp>
#include <gtest/gtest.h>
#include <sstream>

using namespace gtirb;

TEST(Unit_Addr, ctor_0) {
  EXPECT_NO_THROW(Addr());
  EXPECT_EQ(Addr(BadAddress), Addr());
}

TEST(Unit_Addr, ctor_1) {
  EXPECT_NO_THROW(Addr(2112));

  auto Ea = Addr(2112);
  EXPECT_EQ(uint64_t(2112), Ea.get());
}

TEST(Unit_Addr, fromIntegers) {
  uint64_t U64 = 1;
  Addr U64Addr(U64);
  EXPECT_EQ(U64Addr.get(), U64);

  int64_t S64 = 2;
  Addr S64Addr(S64);
  EXPECT_EQ(S64Addr.get(), S64);

  uint32_t U32 = 3;
  Addr U32Addr(U32);
  EXPECT_EQ(U32Addr.get(), U32);

  int32_t S32 = 4;
  Addr S32Addr(S32);
  EXPECT_EQ(S32Addr.get(), S32);

  uint16_t U16 = 5;
  Addr U16Addr(U16);
  EXPECT_EQ(U16Addr.get(), U16);

  int16_t S16 = 6;
  Addr S16Addr(S16);
  EXPECT_EQ(S16Addr.get(), S16);

  uint8_t U8 = 7;
  Addr U8Addr(U8);
  EXPECT_EQ(U8Addr.get(), U8);

  int8_t S8 = 8;
  Addr S8Addr(S8);
  EXPECT_EQ(S8Addr.get(), S8);
}

TEST(Unit_Addr, toIntegers) {
  Addr Ea(1);
  EXPECT_EQ(static_cast<uint64_t>(Ea), static_cast<uint64_t>(1));
  EXPECT_EQ(static_cast<int64_t>(Ea), static_cast<int64_t>(1));

  EXPECT_EQ(static_cast<uint32_t>(Ea), static_cast<uint32_t>(1));
  EXPECT_EQ(static_cast<int32_t>(Ea), static_cast<int32_t>(1));

  EXPECT_EQ(static_cast<uint16_t>(Ea), static_cast<uint16_t>(1));
  EXPECT_EQ(static_cast<int16_t>(Ea), static_cast<int16_t>(1));

  EXPECT_EQ(static_cast<uint8_t>(Ea), static_cast<uint8_t>(1));
  EXPECT_EQ(static_cast<int8_t>(Ea), static_cast<int8_t>(1));
}

TEST(Unit_Addr, comparison) {
  auto Ea1 = Addr(2112);
  auto Ea2 = Addr(1221);

  EXPECT_GT(Ea1, Ea2);
  EXPECT_TRUE(Ea1 > Ea2);

  EXPECT_NE(Ea1, Ea2);
  EXPECT_TRUE(Ea1 != Ea2);

  EXPECT_LT(Ea2, Ea1);
  EXPECT_TRUE(Ea2 < Ea1);

  EXPECT_FALSE(Ea1 == Ea2);
}

TEST(Unit_Addr, arithmetic) {
  Addr Ea(10);

  EXPECT_EQ(Ea + Addr(5), Addr(15));
  EXPECT_EQ(Ea + uint64_t(5), Addr(15));
  EXPECT_EQ(Ea - Addr(5), Addr(5));
  EXPECT_EQ(Ea - uint64_t(5), Addr(5));

  Addr Ea2(10);
  EXPECT_EQ(Ea2 += Addr(5), Addr(15));
  EXPECT_EQ(Ea2, Addr(15));

  Addr Ea3(10);
  EXPECT_EQ(Ea3 += uint64_t(5), Addr(15));
  EXPECT_EQ(Ea3, Addr(15));

  Addr Ea4(10);
  EXPECT_EQ(Ea4 -= Addr(5), Addr(5));
  EXPECT_EQ(Ea4, Addr(5));

  Addr Ea5(10);
  EXPECT_EQ(Ea5 -= uint64_t(5), Addr(5));
  EXPECT_EQ(Ea5, Addr(5));
}

TEST(Unit_Addr, set) {
  auto Ea1 = Addr(2112);
  auto Ea2 = Addr();

  Ea2.set(uint64_t(2112));

  EXPECT_EQ(Ea1, Ea2);
  EXPECT_EQ(uint64_t(2112), Ea2.get());
}

TEST(Unit_Addr, ostream) {
  std::ostringstream Os;

  Os << 123 << " " << Addr(456) << " 789";
  EXPECT_EQ(Os.str(), "123 0x1c8 789");
}
