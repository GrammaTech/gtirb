#include <gtirb/EA.hpp>
#include <gtest/gtest.h>
#include <sstream>

using namespace gtirb;

TEST(Unit_EA, ctor_0) {
  EXPECT_NO_THROW(EA());
  EXPECT_EQ(EA(constants::BadAddress), EA());
}

TEST(Unit_EA, ctor_1) {
  EXPECT_NO_THROW(EA(2112));

  auto ea = EA(2112);
  EXPECT_EQ(uint64_t(2112), ea.get());
}

TEST(Unit_EA, fromIntegers) {
  uint64_t u64 = 1;
  EA u64EA(u64);
  EXPECT_EQ(u64EA.get(), u64);

  int64_t s64 = 2;
  EA s64EA(s64);
  EXPECT_EQ(s64EA.get(), s64);

  uint32_t u32 = 3;
  EA u32EA(u32);
  EXPECT_EQ(u32EA.get(), u32);

  int32_t s32 = 4;
  EA s32EA(s32);
  EXPECT_EQ(s32EA.get(), s32);

  uint16_t u16 = 5;
  EA u16EA(u16);
  EXPECT_EQ(u16EA.get(), u16);

  int16_t s16 = 6;
  EA s16EA(s16);
  EXPECT_EQ(s16EA.get(), s16);

  uint8_t u8 = 7;
  EA u8EA(u8);
  EXPECT_EQ(u8EA.get(), u8);

  int8_t s8 = 8;
  EA s8EA(s8);
  EXPECT_EQ(s8EA.get(), s8);
}

TEST(Unit_EA, toIntegers) {
  EA ea(1);
  EXPECT_EQ(static_cast<uint64_t>(ea), static_cast<uint64_t>(1));
  EXPECT_EQ(static_cast<int64_t>(ea), static_cast<int64_t>(1));

  EXPECT_EQ(static_cast<uint32_t>(ea), static_cast<uint32_t>(1));
  EXPECT_EQ(static_cast<int32_t>(ea), static_cast<int32_t>(1));

  EXPECT_EQ(static_cast<uint16_t>(ea), static_cast<uint16_t>(1));
  EXPECT_EQ(static_cast<int16_t>(ea), static_cast<int16_t>(1));

  EXPECT_EQ(static_cast<uint8_t>(ea), static_cast<uint8_t>(1));
  EXPECT_EQ(static_cast<int8_t>(ea), static_cast<int8_t>(1));
}

TEST(Unit_EA, comparison) {
  auto ea1 = EA(2112);
  auto ea2 = EA(1221);

  EXPECT_GT(ea1, ea2);
  EXPECT_TRUE(ea1 > ea2);

  EXPECT_NE(ea1, ea2);
  EXPECT_TRUE(ea1 != ea2);

  EXPECT_LT(ea2, ea1);
  EXPECT_TRUE(ea2 < ea1);

  EXPECT_FALSE(ea1 == ea2);
}

TEST(Unit_EA, arithmetic) {
  EA ea(10);

  EXPECT_EQ(ea + EA(5), EA(15));
  EXPECT_EQ(ea + uint64_t(5), EA(15));
  EXPECT_EQ(ea - EA(5), EA(5));
  EXPECT_EQ(ea - uint64_t(5), EA(5));

  EA ea2(10);
  EXPECT_EQ(ea2 += EA(5), EA(15));
  EXPECT_EQ(ea2, EA(15));

  EA ea3(10);
  EXPECT_EQ(ea3 += uint64_t(5), EA(15));
  EXPECT_EQ(ea3, EA(15));

  EA ea4(10);
  EXPECT_EQ(ea4 -= EA(5), EA(5));
  EXPECT_EQ(ea4, EA(5));

  EA ea5(10);
  EXPECT_EQ(ea5 -= uint64_t(5), EA(5));
  EXPECT_EQ(ea5, EA(5));
}

TEST(Unit_EA, set) {
  auto ea1 = EA(2112);
  auto ea2 = EA();

  ea2.set(uint64_t(2112));

  EXPECT_EQ(ea1, ea2);
  EXPECT_EQ(uint64_t(2112), ea2.get());
}

TEST(Unit_EA, ostream) {
  std::ostringstream os;

  os << 123 << " " << EA(456) << " 789";
  EXPECT_EQ(os.str(), "123 0x1c8 789");
}
