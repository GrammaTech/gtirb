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
#include "TestHelpers.hpp"
#include <gtirb/Context.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/proto/Section.pb.h>
#include <gtest/gtest.h>
#include <limits>
#include <sstream>

using namespace gtirb;

static Context Ctx;

TEST(Unit_Section, compilationIteratorTypes) {
  static_assert(std::is_same_v<Section::block_iterator::reference, Node&>);
  static_assert(
      std::is_same_v<Section::const_block_iterator::reference, const Node&>);
  static_assert(
      std::is_same_v<Section::block_subrange::iterator::reference, Node&>);
  static_assert(
      std::is_same_v<Section::const_block_subrange::iterator::reference,
                     const Node&>);

  {
    Section::block_iterator BIt;
    Section::const_block_iterator CBIt = BIt;
    CBIt = BIt;

    Section::block_range BRng;
    Section::const_block_range CBRng = BRng;
    CBRng = BRng;
  }
  static_assert(!std::is_convertible_v<Section::const_block_iterator,
                                       Section::block_iterator>);
  static_assert(
      !std::is_convertible_v<Section::const_block_range, Section::block_range>);
}

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
  auto* BI2 = S->addByteInterval(Ctx, Addr(8), 12);
  auto* BI3 = S->addByteInterval(Ctx, Addr(13), 8);
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

  // Query is correct for ByteIntervals with overlapping start addresses.

  BI1->setAddress(Addr(13));
  Range = S->findByteIntervalsOn(Addr(14));
  ASSERT_EQ(std::distance(Range.begin(), Range.end()), 3);
  EXPECT_EQ(&*std::next(Range.begin(), 0), BI2);
  EXPECT_EQ(&*std::next(Range.begin(), 1), BI3);
  EXPECT_EQ(&*std::next(Range.begin(), 2), BI1);

  ConstRange = CS->findByteIntervalsOn(Addr(14));
  ASSERT_EQ(std::distance(ConstRange.begin(), ConstRange.end()), 3);
  EXPECT_EQ(&*std::next(ConstRange.begin(), 0), BI2);
  EXPECT_EQ(&*std::next(ConstRange.begin(), 1), BI3);
  EXPECT_EQ(&*std::next(ConstRange.begin(), 2), BI1);

  // Moving a ByteInterval does not leave duplicates in codomain tree.

  BI2->setAddress(Addr(13));
  Range = S->findByteIntervalsOn(Addr(14));
  ASSERT_EQ(std::distance(Range.begin(), Range.end()), 3);
  EXPECT_EQ(&*std::next(Range.begin(), 0), BI3);
  EXPECT_EQ(&*std::next(Range.begin(), 1), BI2);
  EXPECT_EQ(&*std::next(Range.begin(), 2), BI1);

  ConstRange = CS->findByteIntervalsOn(Addr(14));
  ASSERT_EQ(std::distance(ConstRange.begin(), ConstRange.end()), 3);
  EXPECT_EQ(&*std::next(ConstRange.begin(), 0), BI3);
  EXPECT_EQ(&*std::next(ConstRange.begin(), 1), BI2);
  EXPECT_EQ(&*std::next(ConstRange.begin(), 2), BI1);

  // Removing a ByteInterval with an overlapping address does not remove others.

  BI3->setAddress(std::nullopt);
  Range = S->findByteIntervalsOn(Addr(14));
  ASSERT_EQ(std::distance(Range.begin(), Range.end()), 2);
  EXPECT_EQ(&*std::next(Range.begin(), 0), BI2);
  EXPECT_EQ(&*std::next(Range.begin(), 1), BI1);

  ConstRange = CS->findByteIntervalsOn(Addr(14));
  ASSERT_EQ(std::distance(ConstRange.begin(), ConstRange.end()), 2);
  EXPECT_EQ(&*std::next(ConstRange.begin(), 0), BI2);
  EXPECT_EQ(&*std::next(ConstRange.begin(), 1), BI1);
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

  Range = S->findByteIntervalsAt(Addr(0), Addr(static_cast<uint64_t>(-1)));
  ASSERT_EQ(std::distance(Range.begin(), Range.end()), 1);
  EXPECT_EQ(&*Range.begin(), BI2);

  ConstRange =
      CS->findByteIntervalsAt(Addr(0), Addr(static_cast<uint64_t>(-1)));
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

TEST(Unit_Section, findBlocksOn) {
  auto* S = Section::Create(Ctx, "test");
  auto* BI1 = S->addByteInterval(Ctx, 16);
  auto* CB11 = BI1->addBlock<CodeBlock>(Ctx, 0, 4);
  auto* BI2 = S->addByteInterval(Ctx, Addr(0), 10);
  auto* CB21 = BI2->addBlock<CodeBlock>(Ctx, 0, 2);
  auto* CB22 = BI2->addBlock<CodeBlock>(Ctx, 5, 2);
  auto* BI3 = S->addByteInterval(Ctx, Addr(4), 4);
  auto* CB31 = BI3->addBlock<CodeBlock>(Ctx, 0, 3);
  const Section* CS = S;

  // Querying an out-of-bounds offset produces an empty range.

  auto BlockRange = S->findBlocksOn(Addr(10));
  EXPECT_TRUE(BlockRange.empty());

  auto ConstBlockRange = CS->findBlocksOn(Addr(10));
  EXPECT_TRUE(ConstBlockRange.empty());

  // Querying an in-bounds offset returns the appropriate blocks from all
  // ByteIntervals.

  BlockRange = S->findBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 2);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB22);

  ConstBlockRange = CS->findBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 2);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB22);

  // Assigning a ByteInterval address may change the query results.

  BI1->setAddress(Addr(4));

  BlockRange = S->findBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 3);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB11);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 2), CB22);

  ConstBlockRange = CS->findBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 3);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB11);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 2), CB22);

  // Changing a ByteInterval's address may change the query results.

  BI2->setAddress(Addr(4));

  BlockRange = S->findBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 3);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB31);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 2), CB11);

  ConstBlockRange = CS->findBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 3);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB31);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 2), CB11);

  BI3->setAddress(Addr(0));

  BlockRange = S->findBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 2);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB11);

  ConstBlockRange = CS->findBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 2);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB11);
}

TEST(Unit_Section, findCodeBlocksOn) {
  auto* S = Section::Create(Ctx, "test");
  auto* BI1 = S->addByteInterval(Ctx, 16);
  auto* CB11 = BI1->addBlock<CodeBlock>(Ctx, 0, 4);
  auto* BI2 = S->addByteInterval(Ctx, Addr(0), 10);
  auto* CB21 = BI2->addBlock<CodeBlock>(Ctx, 0, 2);
  auto* CB22 = BI2->addBlock<CodeBlock>(Ctx, 5, 2);
  auto* BI3 = S->addByteInterval(Ctx, Addr(4), 4);
  auto* CB31 = BI3->addBlock<CodeBlock>(Ctx, 0, 3);
  const Section* CS = S;

  // Querying an out-of-bounds offset produces an empty range.

  auto BlockRange = S->findCodeBlocksOn(Addr(10));
  EXPECT_TRUE(BlockRange.empty());

  auto ConstBlockRange = CS->findCodeBlocksOn(Addr(10));
  EXPECT_TRUE(ConstBlockRange.empty());

  // Querying an in-bounds offset returns the appropriate blocks from all
  // ByteIntervals.

  BlockRange = S->findCodeBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 2);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB22);

  ConstBlockRange = CS->findCodeBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 2);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB22);

  // Assigning a ByteInterval address may change the query results.

  BI1->setAddress(Addr(4));

  BlockRange = S->findCodeBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 3);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB11);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 2), CB22);

  ConstBlockRange = CS->findCodeBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 3);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB11);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 2), CB22);

  // Changing a ByteInterval's address may change the query results.

  BI2->setAddress(Addr(4));

  BlockRange = S->findCodeBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 3);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB31);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 2), CB11);

  ConstBlockRange = CS->findCodeBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 3);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB31);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 2), CB11);

  BI3->setAddress(Addr(0));

  BlockRange = S->findCodeBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 2);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB11);

  ConstBlockRange = CS->findCodeBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 2);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB11);
}

TEST(Unit_Section, findDataBlocksOn) {
  auto* S = Section::Create(Ctx, "test");
  auto* BI1 = S->addByteInterval(Ctx, 16);
  auto* CB11 = BI1->addBlock<DataBlock>(Ctx, 0, 4);
  auto* BI2 = S->addByteInterval(Ctx, Addr(0), 10);
  auto* CB21 = BI2->addBlock<DataBlock>(Ctx, 0, 2);
  auto* CB22 = BI2->addBlock<DataBlock>(Ctx, 5, 2);
  auto* BI3 = S->addByteInterval(Ctx, Addr(4), 4);
  auto* CB31 = BI3->addBlock<DataBlock>(Ctx, 0, 3);
  const Section* CS = S;

  // Querying an out-of-bounds offset produces an empty range.

  auto BlockRange = S->findDataBlocksOn(Addr(10));
  EXPECT_TRUE(BlockRange.empty());

  auto ConstBlockRange = CS->findDataBlocksOn(Addr(10));
  EXPECT_TRUE(ConstBlockRange.empty());

  // Querying an in-bounds offset returns the appropriate blocks from all
  // ByteIntervals.

  BlockRange = S->findDataBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 2);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB22);

  ConstBlockRange = CS->findDataBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 2);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB22);

  // Assigning a ByteInterval address may change the query results.

  BI1->setAddress(Addr(4));

  BlockRange = S->findDataBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 3);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB11);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 2), CB22);

  ConstBlockRange = CS->findDataBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 3);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB11);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 2), CB22);

  // Changing a ByteInterval's address may change the query results.

  BI2->setAddress(Addr(4));

  BlockRange = S->findDataBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 3);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB31);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 2), CB11);

  ConstBlockRange = CS->findDataBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 3);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB31);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 2), CB11);

  BI3->setAddress(Addr(0));

  BlockRange = S->findDataBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 2);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB11);

  ConstBlockRange = CS->findDataBlocksOn(Addr(5));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 2);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB11);
}

TEST(Unit_Section, findBlocksAt) {
  auto* S = Section::Create(Ctx, "test");
  auto* BI1 = S->addByteInterval(Ctx, 16);
  auto* CB11 = BI1->addBlock<CodeBlock>(Ctx, 0, 4);
  auto* BI2 = S->addByteInterval(Ctx, Addr(0), 10);
  auto* CB21 = BI2->addBlock<CodeBlock>(Ctx, 0, 2);
  auto* CB22 = BI2->addBlock<CodeBlock>(Ctx, 5, 2);
  auto* BI3 = S->addByteInterval(Ctx, Addr(4), 4);
  auto* CB31 = BI3->addBlock<CodeBlock>(Ctx, 0, 3);
  const Section* CS = S;

  // Querying an out-of-bounds offset produces an empty range.

  auto BlockRange = S->findBlocksAt(Addr(10), Addr(20));
  EXPECT_TRUE(BlockRange.empty());

  auto ConstBlockRange = CS->findBlocksAt(Addr(10), Addr(20));
  EXPECT_TRUE(ConstBlockRange.empty());

  // Querying an in-bounds offset returns the appropriate blocks from all
  // ByteIntervals.

  BlockRange = S->findBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 2);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB22);

  ConstBlockRange = CS->findBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 2);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB22);

  // Assigning a ByteInterval address may change the query results.

  BI1->setAddress(Addr(4));

  BlockRange = S->findBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 3);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB11);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 2), CB22);

  ConstBlockRange = CS->findBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 3);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB11);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 2), CB22);

  // Changing a ByteInterval's address may change the query results.

  BI2->setAddress(Addr(4));

  BlockRange = S->findBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 3);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB31);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 2), CB11);

  ConstBlockRange = CS->findBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 3);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB31);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 2), CB11);

  BI3->setAddress(Addr(0));

  BlockRange = S->findBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 2);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB11);

  ConstBlockRange = CS->findBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 2);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB11);
}

TEST(Unit_Section, findCodeBlocksAt) {
  auto* S = Section::Create(Ctx, "test");
  auto* BI1 = S->addByteInterval(Ctx, 16);
  auto* CB11 = BI1->addBlock<CodeBlock>(Ctx, 0, 4);
  auto* BI2 = S->addByteInterval(Ctx, Addr(0), 10);
  auto* CB21 = BI2->addBlock<CodeBlock>(Ctx, 0, 2);
  auto* CB22 = BI2->addBlock<CodeBlock>(Ctx, 5, 2);
  auto* BI3 = S->addByteInterval(Ctx, Addr(4), 4);
  auto* CB31 = BI3->addBlock<CodeBlock>(Ctx, 0, 3);
  const Section* CS = S;

  // Querying an out-of-bounds offset produces an empty range.

  auto BlockRange = S->findCodeBlocksAt(Addr(10), Addr(20));
  EXPECT_TRUE(BlockRange.empty());

  auto ConstBlockRange = CS->findCodeBlocksAt(Addr(10), Addr(20));
  EXPECT_TRUE(ConstBlockRange.empty());

  // Querying an in-bounds offset returns the appropriate blocks from all
  // ByteIntervals.

  BlockRange = S->findCodeBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 2);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB22);

  ConstBlockRange = CS->findCodeBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 2);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB22);

  // Assigning a ByteInterval address may change the query results.

  BI1->setAddress(Addr(4));

  BlockRange = S->findCodeBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 3);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB11);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 2), CB22);

  ConstBlockRange = CS->findCodeBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 3);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB11);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 2), CB22);

  // Changing a ByteInterval's address may change the query results.

  BI2->setAddress(Addr(4));

  BlockRange = S->findCodeBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 3);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB31);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 2), CB11);

  ConstBlockRange = CS->findCodeBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 3);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB31);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 2), CB11);

  BI3->setAddress(Addr(0));

  BlockRange = S->findCodeBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 2);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB11);

  ConstBlockRange = CS->findCodeBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 2);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB11);
}

TEST(Unit_Section, findDataBlocksAt) {
  auto* S = Section::Create(Ctx, "test");
  auto* BI1 = S->addByteInterval(Ctx, 16);
  auto* CB11 = BI1->addBlock<DataBlock>(Ctx, 0, 4);
  auto* BI2 = S->addByteInterval(Ctx, Addr(0), 10);
  auto* CB21 = BI2->addBlock<DataBlock>(Ctx, 0, 2);
  auto* CB22 = BI2->addBlock<DataBlock>(Ctx, 5, 2);
  auto* BI3 = S->addByteInterval(Ctx, Addr(4), 4);
  auto* CB31 = BI3->addBlock<DataBlock>(Ctx, 0, 3);
  const Section* CS = S;

  // Querying an out-of-bounds offset produces an empty range.

  auto BlockRange = S->findDataBlocksAt(Addr(10), Addr(20));
  EXPECT_TRUE(BlockRange.empty());

  auto ConstBlockRange = CS->findDataBlocksAt(Addr(10), Addr(20));
  EXPECT_TRUE(ConstBlockRange.empty());

  // Querying an in-bounds offset returns the appropriate blocks from all
  // ByteIntervals.

  BlockRange = S->findDataBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 2);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB22);

  ConstBlockRange = CS->findDataBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 2);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB22);

  // Assigning a ByteInterval address may change the query results.

  BI1->setAddress(Addr(4));

  BlockRange = S->findDataBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 3);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB11);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 2), CB22);

  ConstBlockRange = CS->findDataBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 3);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB31);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB11);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 2), CB22);

  // Changing a ByteInterval's address may change the query results.

  BI2->setAddress(Addr(4));

  BlockRange = S->findDataBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 3);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB31);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 2), CB11);

  ConstBlockRange = CS->findDataBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 3);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB31);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 2), CB11);

  BI3->setAddress(Addr(0));

  BlockRange = S->findDataBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 2);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), CB11);

  ConstBlockRange = CS->findDataBlocksAt(Addr(3), Addr(6));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 2);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), CB21);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), CB11);
}

TEST(Unit_Section, testIterationOrder) {
  auto* S = Section::Create(Ctx, "test");
  auto* BI1 = S->addByteInterval(Ctx, Addr(0));
  auto* CB11 = BI1->addBlock<CodeBlock>(Ctx, 0, 0);
  auto* CB12 = BI1->addBlock<CodeBlock>(Ctx, 0, 1);
  auto* BI2 = S->addByteInterval(Ctx, Addr(0));
  auto* DB21 = BI2->addBlock<DataBlock>(Ctx, 0, 0);
  auto* DB22 = BI2->addBlock<DataBlock>(Ctx, 0, 1);
  auto* BI3 = S->addByteInterval(Ctx, Addr(1));
  auto* DB31 = BI3->addBlock<DataBlock>(Ctx, 0, 0);
  auto* DB32 = BI3->addBlock<DataBlock>(Ctx, 0, 1);

  {
    std::vector<Node*> ExpectedOrder = {CB11, DB21, CB12, DB22, DB31, DB32};
    EXPECT_EQ(pointers(S->blocks()), ExpectedOrder);
  }
}
