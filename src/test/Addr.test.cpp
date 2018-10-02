//===- Addr.test.cpp --------------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2018 GrammaTech, Inc.
//
//  This code is licensed under the MIT license. See the LICENSE file in the
//  project root for license terms.
//
//  This project is sponsored by the Office of Naval Research, One Liberty
//  Center, 875 N. Randolph Street, Arlington, VA 22203 under contract #
//  N68335-17-C-0700.  The content of the information does not necessarily
//  reflect the position or policy of the Government and no official
//  endorsement should be inferred.
//
//===----------------------------------------------------------------------===//
#include <gtirb/Addr.hpp>
#include <gtest/gtest.h>
#include <sstream>

using namespace gtirb;

TEST(Unit_Addr, ctor_0) { EXPECT_EQ(Addr(), Addr()); }

TEST(Unit_Addr, ctor_1) {
  auto Ea = Addr(2112);
  EXPECT_EQ(2112, uint64_t(Ea));
}

TEST(Unit_Addr, fromIntegers) {
  uint64_t U64 = 1;
  Addr U64Addr(U64);
  EXPECT_EQ(uint64_t(U64Addr), U64);

  int64_t S64 = 2;
  Addr S64Addr(S64);
  EXPECT_EQ(uint64_t(S64Addr), S64);
}

TEST(Unit_Addr, toIntegers) {
  Addr Ea(1);
  EXPECT_EQ(static_cast<uint64_t>(Ea), static_cast<uint64_t>(1));
  EXPECT_EQ(static_cast<uint64_t>(Ea), static_cast<int64_t>(1));
}

TEST(Unit_Addr, comparison) {
  Addr Ea1(2112), Ea2(1221), Ea3(1000), Ea4(1000);

  EXPECT_EQ(Ea3, Ea4);
  EXPECT_TRUE(Ea3 == Ea4);
  EXPECT_FALSE(Ea3 != Ea4);

  EXPECT_NE(Ea1, Ea2);
  EXPECT_TRUE(Ea1 != Ea2);
  EXPECT_FALSE(Ea1 == Ea2);

  EXPECT_LT(Ea2, Ea1);
  EXPECT_TRUE(Ea2 < Ea1);
  EXPECT_FALSE(Ea2 > Ea1);

  EXPECT_GT(Ea1, Ea2);
  EXPECT_TRUE(Ea1 > Ea2);
  EXPECT_FALSE(Ea1 < Ea2);

  EXPECT_LE(Ea3, Ea4);
  EXPECT_TRUE(Ea3 <= Ea4);
  EXPECT_FALSE(Ea3 > Ea4);

  EXPECT_GE(Ea3, Ea4);
  EXPECT_TRUE(Ea3 >= Ea4);
  EXPECT_FALSE(Ea3 < Ea4);
}

TEST(Unit_Addr, arithmetic) {
  Addr Ea(10);

  EXPECT_EQ(Ea + 5, Addr(15));
  EXPECT_EQ(Ea - Addr(5), 5);
  EXPECT_EQ(Ea - 5, Addr(5));

  EXPECT_EQ(++Ea, Addr(11));
  EXPECT_EQ(--Ea, Addr(10));

  EXPECT_EQ(Ea++, Addr(10));
  EXPECT_EQ(Ea, Addr(11));

  EXPECT_EQ(Ea--, Addr(11));
  EXPECT_EQ(Ea, Addr(10));

  EXPECT_EQ(Ea += 5, Addr(15));
  EXPECT_EQ(Ea -= 5, Addr(10));
}

TEST(Unit_Addr, ostream) {
  std::ostringstream Os;

  Os << 123 << " 0x" << std::hex << static_cast<uint64_t>(Addr(456)) << std::dec
     << " 789";
  EXPECT_EQ(Os.str(), "123 0x1c8 789");
}

TEST(Unit_Addr, Constexpr) {
  // Test constexpr equality.
  static_assert(Addr(10) == Addr(10));
  static_assert(noexcept(Addr(10) == Addr(10)));
  static_assert(Addr(10) != Addr(1));
  static_assert(noexcept(Addr(10) != Addr(1)));

  // Test constexpr conversion to int.
  static_assert((uint64_t)Addr(10) == 10);
  static_assert(noexcept((uint64_t)Addr(10)));

  // Test constexpr comparisons.
  static_assert(Addr(1) < Addr(10));
  static_assert(noexcept(Addr(1) < Addr(10)));
  static_assert(Addr(10) > Addr(1));
  static_assert(noexcept(Addr(10) > Addr(1)));
  static_assert(Addr(2) <= Addr(2));
  static_assert(noexcept(Addr(2) <= Addr(2)));
  static_assert(Addr(2) >= Addr(2));
  static_assert(noexcept(Addr(2) >= Addr(2)));

  // Test constexpr increment and decrement.
  static_assert(++Addr(2) == Addr(3));
  static_assert(noexcept(++Addr(2)));
  static_assert(--Addr(2) == Addr(1));
  static_assert(noexcept(--Addr(2)));
  static_assert(Addr(2)++ == Addr(2));
  static_assert(noexcept(Addr(2)++));
  static_assert(Addr(2)-- == Addr(2));
  static_assert(noexcept(Addr(2)--));

  // Test unary operators.
  static_assert(+Addr(12) == Addr(12));
  static_assert(noexcept(+Addr(12)));
  static_assert(~Addr(0) == Addr(std::numeric_limits<Addr::value_type>::max()));
  static_assert(noexcept(~Addr(0)));

  // Test arithmetic operators.
  static_assert(Addr(10) + 1 == Addr(11));
  static_assert(noexcept(Addr(10) + 1));
  static_assert(1 + Addr(10) == Addr(11));
  static_assert(noexcept(1 + Addr(10)));
  static_assert(Addr(10) - 1 == Addr(9));
  static_assert(noexcept(Addr(10) - 1));
  static_assert(Addr(10) - Addr(9) == 1);
  static_assert(noexcept(Addr(10) - Addr(9)));

  // Test arithmetic compound assignment operators.
  static_assert((Addr(10) += 1) == Addr(11));
  static_assert(noexcept(Addr(10) += 1));
  static_assert((Addr(10) -= 1) == Addr(9));
  static_assert(noexcept(Addr(10) -= 1));

  // Ensure that wrapping happens at compile time, in either direction, without
  // triggering undefined behavior. Note, this explicitly disables compiler
  // diagnostics about overflow as those may be warned on even with well-defined
  // semantics.
#if defined(_MSC_VER)
#pragma warning(push)
#pragma warning(disable : 4307) // '+': integral constant overflow
#endif
  static_assert(Addr(0) - 1 ==
                Addr(std::numeric_limits<Addr::value_type>::max()));
  static_assert(noexcept(Addr(0) - 1));
  static_assert(Addr(std::numeric_limits<Addr::value_type>::max()) + 1 ==
                Addr(0));
  static_assert(
      noexcept(Addr(std::numeric_limits<Addr::value_type>::max()) + 1));
#if defined(_MSC_VER)
#pragma warning(pop)
#endif
}
