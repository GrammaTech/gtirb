//===- ExtensibleEnum.test.cpp ----------------------------------*- C++ -*-===//
//
//  Copyright (C) 2020 GrammaTech, Inc.
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
#include <gtirb/ExtensibleEnum.hpp>
#include <gtest/gtest.h>

using namespace gtirb;

class TestIntEnum : public ExtensibleEnum<> {
public:
  using ExtensibleEnum::ExtensibleEnum;

  static const TestIntEnum One;
  static const TestIntEnum Two;
};
constexpr TestIntEnum TestIntEnum::One{1};
constexpr TestIntEnum TestIntEnum::Two{2};

class TestShortEnum : public ExtensibleEnum<short> {
public:
  using ExtensibleEnum::ExtensibleEnum;

  static const TestShortEnum Apple;
  static const TestShortEnum Orange;
};
constexpr TestShortEnum TestShortEnum::Apple{1};
constexpr TestShortEnum TestShortEnum::Orange{2};

class ExtendedEnum : public TestIntEnum {
public:
  using TestIntEnum::TestIntEnum;
  constexpr ExtendedEnum(const TestIntEnum& Other) : TestIntEnum(Other) {}

  static const ExtendedEnum Three;
  static const ExtendedEnum Four;
};
constexpr ExtendedEnum ExtendedEnum::Three{3};
constexpr ExtendedEnum ExtendedEnum::Four{4};

TEST(Unit_ExtensibleEnum, ctors) {
  EXPECT_EQ(TestIntEnum{}, TestIntEnum{});

  TestIntEnum E1 = TestIntEnum::One;
  TestIntEnum E2{2};
  EXPECT_EQ(TestIntEnum::One, E1);
  EXPECT_EQ(2, static_cast<int>(E2));

  ExtendedEnum E3 = ExtendedEnum::Three;
  EXPECT_NE(E1, E3);
}

TEST(Unit_ExtensibleEnum, ctors_that_should_not_compile) {
  // TestIntEnum E1 = 12;
  // TestIntEnum E2 = TestShortEnum::Apple;
  // TestIntEnum E3{TestShortEnum::Apple};
  // TestShortEnum E4{70000};

  //// FIXME: paren init should be disabled, but I know of no way to do it that
  //// continues to allow brace initialization.
  // TestShortEnum E5(2);
}

TEST(Unit_ExtensibleEnum, assignment) {
  TestIntEnum E1 = TestIntEnum::Two;
  TestIntEnum E2;
  ExtendedEnum E3;

  E2 = E1;
  EXPECT_EQ(E2, E1);
  EXPECT_EQ(static_cast<int>(TestIntEnum::Two), static_cast<int>(E2));

  E1 = (TestIntEnum)12;
  EXPECT_EQ(12, static_cast<int>(E1));

  E3 = TestIntEnum::One;
  E1 = ExtendedEnum::Four;
}

TEST(Unit_ExtensibleEnum, assignments_that_should_not_compile) {
  // TestIntEnum E1;
  // TestShortEnum E2;

  // E1 = 12;
  // E2 = E1;
  // E1 = E2;

  // int i;
  // i = E1;
}

TEST(Unit_ExtensibleEnum, comparison) {
  TestIntEnum E1 = TestIntEnum::One, E2 = TestIntEnum::Two;
  ExtendedEnum Extended = E1;

  EXPECT_TRUE(E1 < E2);
  EXPECT_TRUE(E1 <= E2);
  EXPECT_TRUE(E2 > E1);
  EXPECT_TRUE(E2 >= E1);
  EXPECT_TRUE(E1 == Extended);
  EXPECT_TRUE(Extended != E2);
  EXPECT_TRUE(Extended <= E1);
}

TEST(Unit_ExtensibleEnum, comparisons_that_should_not_compile) {
  // TestIntEnum E1{1};
  // TestShortEnum E2{0};

  // bool b1 = E1 < E2;
  // bool b2 = E1 == E2;
  // bool b3 = E1 == 1;
}

void FuncAcceptingTestIntEnum(TestIntEnum E) {}
void FuncAcceptingExtendedEnum(ExtendedEnum E) {}
void FuncAcceptingTestShortEnum(TestShortEnum E) {}
void FuncAcceptingInt(int E) {}

TEST(Unit_ExtensibleEnum, conversion) {
  TestIntEnum E1;
  ExtendedEnum E2;

  FuncAcceptingExtendedEnum(E1);
  FuncAcceptingTestIntEnum(E2);
}

TEST(Unit_ExtensibleEnum, conversions_that_should_not_compile) {
  // TestIntEnum E1;
  // int i, j = E1;
  // i = E1;

  // FuncAcceptingInt(E1);
  // FuncAcceptingTestShortEnum(E1);
}

TEST(Unit_ExtensibleEnum, operators_that_should_not_compile) {
  // TestIntEnum E1, E2;

  // E1 = E1 | E2;
  // E1 = E1 & E2;
  // E1 = ~E1;
}

TEST(Unit_ExtensibleEnum, Constexpr) {
  static_assert(TestIntEnum{TestIntEnum::One} == TestIntEnum::One);
  static_assert(static_cast<int>(TestShortEnum{TestShortEnum::Apple}) == 1);
  static_assert(TestIntEnum{12} < TestIntEnum{42});
}
