//===- Section.test.cpp -----------------------------------------*- C++ -*-===//
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
#include "SerializationTestHarness.hpp"
#include <gtirb/Context.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/proto/Section.pb.h>
#include <gtest/gtest.h>
#include <limits>
#include <sstream>

using namespace gtirb;

static Context Ctx;

TEST(Unit_Section, noCopyMoveConstructors) {
  EXPECT_FALSE(std::is_copy_constructible_v<Section>);
  EXPECT_FALSE(std::is_move_constructible_v<Section>);
  EXPECT_FALSE(std::is_copy_assignable_v<Section>);
  EXPECT_FALSE(std::is_move_assignable_v<Section>);
}

TEST(Unit_Section, getAddress) {
  using OAddr = std::optional<Addr>;
  using OSize = std::optional<uint64_t>;

  auto* S = Section::Create(Ctx, "test");
  EXPECT_EQ(S->getAddress(), OAddr());
  EXPECT_EQ(S->getSize(), OSize());

  S->addByteInterval(Ctx, Addr(5), 10);
  EXPECT_EQ(S->getAddress(), OAddr(Addr(5)));
  EXPECT_EQ(S->getSize(), OSize(10));

  S->addByteInterval(Ctx, Addr(15), 10);
  EXPECT_EQ(S->getAddress(), OAddr(Addr(5)));
  EXPECT_EQ(S->getSize(), OSize(20));

  S->addByteInterval(Ctx, OAddr(), 10);
  EXPECT_EQ(S->getAddress(), OAddr());
  EXPECT_EQ(S->getSize(), OSize());
}

TEST(Unit_Section, flags) {
  auto* S = Section::Create(Ctx, "test");
  EXPECT_FALSE(S->isFlagSet(SectionFlag::Undefined));

  S->addFlag(SectionFlag::Executable);
  EXPECT_TRUE(S->isFlagSet(SectionFlag::Executable));

  S->removeFlag(SectionFlag::Executable);
  EXPECT_FALSE(S->isFlagSet(SectionFlag::Executable));

  S->addFlags(SectionFlag::Initialized, SectionFlag::Loaded);
  if (*S->flags_begin() == SectionFlag::Initialized)
    EXPECT_EQ(SectionFlag::Loaded, *(++S->flags_begin()));
  else {
    EXPECT_EQ(SectionFlag::Loaded, *S->flags_begin());
    EXPECT_EQ(SectionFlag::Initialized, *(++S->flags_begin()));
  }
}

TEST(Unit_Section, protobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  std::stringstream ss;

  {
    Context InnerCtx;
    auto* Original = Section::Create(InnerCtx, "name");
    Original->addFlags(SectionFlag::Executable, SectionFlag::Loaded,
                       SectionFlag::Writable);
    STH::save(*Original, ss);
  }
  auto* Result = STH::load<Section>(Ctx, ss);

  EXPECT_EQ(Result->getName(), "name");
  EXPECT_TRUE(Result->isFlagSet(SectionFlag::Executable));
  EXPECT_TRUE(Result->isFlagSet(SectionFlag::Loaded));
  EXPECT_TRUE(Result->isFlagSet(SectionFlag::Writable));
  EXPECT_FALSE(Result->isFlagSet(SectionFlag::Initialized));
  EXPECT_FALSE(Result->isFlagSet(SectionFlag::Readable));
  EXPECT_FALSE(Result->isFlagSet(SectionFlag::ThreadLocal));
}

TEST(Unit_Section, findByteIntervalsOn) {
  auto* S = Section::Create(Ctx, "test");
  auto* BI1 = S->addByteInterval(Ctx, 16);
  auto* BI2 = S->addByteInterval(Ctx, Addr(8), 16);
  const Section* CS = S;

  // Querying an out-of-bounds address returns an empty range.

  auto Range = S->findByteIntervalsOn(Addr(0));
  EXPECT_TRUE(Range.empty());

  auto ConstRange = CS->findByteIntervalsOn(Addr(0));
  EXPECT_TRUE(Range.empty());

  // Querying a in-bounds address returns the correct range.

  Range = S->findByteIntervalsOn(Addr(12));
  ASSERT_EQ(std::distance(Range.begin(), Range.end()), 1);
  EXPECT_EQ(&*Range.begin(), BI2);

  ConstRange = CS->findByteIntervalsOn(Addr(12));
  ASSERT_EQ(std::distance(ConstRange.begin(), ConstRange.end()), 1);
  EXPECT_EQ(&*ConstRange.begin(), BI2);

  // Query returns correct set after addresses are updated.

  BI1->setAddress(Addr(0));
  Range = S->findByteIntervalsOn(Addr(0));
  ASSERT_EQ(std::distance(Range.begin(), Range.end()), 1);
  EXPECT_EQ(&*Range.begin(), BI1);

  ConstRange = CS->findByteIntervalsOn(Addr(0));
  ASSERT_EQ(std::distance(ConstRange.begin(), ConstRange.end()), 1);
  EXPECT_EQ(&*ConstRange.begin(), BI1);

  Range = S->findByteIntervalsOn(Addr(12));
  ASSERT_EQ(std::distance(Range.begin(), Range.end()), 2);
  EXPECT_EQ(&*std::next(Range.begin(), 0), BI1);
  EXPECT_EQ(&*std::next(Range.begin(), 1), BI2);

  ConstRange = CS->findByteIntervalsOn(Addr(12));
  ASSERT_EQ(std::distance(ConstRange.begin(), ConstRange.end()), 2);
  EXPECT_EQ(&*std::next(ConstRange.begin(), 0), BI1);
  EXPECT_EQ(&*std::next(ConstRange.begin(), 1), BI2);
}

TEST(Unit_Section, findByteIntervalsAt) {
  auto* S = Section::Create(Ctx, "test");
  auto* BI1 = S->addByteInterval(Ctx, 16);
  auto* BI2 = S->addByteInterval(Ctx, Addr(8), 16);
  const Section* CS = S;

  // Querying an out-of-bounds address returns an empty range.

  auto Range = S->findByteIntervalsAt(Addr(0));
  EXPECT_TRUE(Range.empty());

  auto ConstRange = CS->findByteIntervalsAt(Addr(0));
  EXPECT_TRUE(ConstRange.empty());

  // Querying exact start address returns the correct interval.

  Range = S->findByteIntervalsAt(Addr(8));
  ASSERT_EQ(std::distance(Range.begin(), Range.end()), 1);
  EXPECT_EQ(&*Range.begin(), BI2);

  ConstRange = CS->findByteIntervalsAt(Addr(8));
  ASSERT_EQ(std::distance(ConstRange.begin(), ConstRange.end()), 1);
  EXPECT_EQ(&*ConstRange.begin(), BI2);

  // Querying a range of addresses returns everything in range.

  Range = S->findByteIntervalsAt(Addr(0), Addr(-1));
  ASSERT_EQ(std::distance(Range.begin(), Range.end()), 1);
  EXPECT_EQ(&*Range.begin(), BI2);

  ConstRange = CS->findByteIntervalsAt(Addr(0), Addr(-1));
  ASSERT_EQ(std::distance(ConstRange.begin(), ConstRange.end()), 1);
  EXPECT_EQ(&*ConstRange.begin(), BI2);

  Range = S->findByteIntervalsAt(Addr(16), Addr(32));
  EXPECT_TRUE(Range.empty());

  ConstRange = CS->findByteIntervalsAt(Addr(16), Addr(32));
  EXPECT_TRUE(ConstRange.empty());

  // Querying an invalid address range returns an empty ByteInterval range.

  Range = S->findByteIntervalsAt(Addr(16), Addr(0));
  EXPECT_TRUE(Range.empty());

  ConstRange = CS->findByteIntervalsAt(Addr(16), Addr(0));
  EXPECT_TRUE(ConstRange.empty());

  // Changing the ByteInterval addresses changes the results.

  BI1->setAddress(Addr(4));
  Range = S->findByteIntervalsAt(Addr(0), Addr(32));
  ASSERT_EQ(std::distance(Range.begin(), Range.end()), 2);
  EXPECT_EQ(&*std::next(Range.begin(), 0), BI1);
  EXPECT_EQ(&*std::next(Range.begin(), 1), BI2);

  ConstRange = CS->findByteIntervalsAt(Addr(0), Addr(32));
  ASSERT_EQ(std::distance(ConstRange.begin(), ConstRange.end()), 2);
  EXPECT_EQ(&*std::next(ConstRange.begin(), 0), BI1);
  EXPECT_EQ(&*std::next(ConstRange.begin(), 1), BI2);
}
