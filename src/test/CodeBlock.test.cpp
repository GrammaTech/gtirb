//===- Block.test.cpp -------------------------------------------*- C++ -*-===//
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
#include <gtirb/ByteInterval.hpp>
#include <gtirb/CodeBlock.hpp>
#include <gtirb/Context.hpp>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

TEST(Unit_CodeBlock, ctor) {
  EXPECT_NE(CodeBlock::Create(Ctx, nullptr, 0), nullptr);
}

TEST(Unit_CodeBlock, getters) {
  auto* BI = ByteInterval::Create(Ctx, nullptr, Addr(0), 2);
  auto* B = BI->addCodeBlock(Ctx, 0, 1, 2);

  EXPECT_EQ(Addr{0}, B->getAddress());
  EXPECT_EQ(uint64_t{1}, B->getSize());
  EXPECT_EQ(uint64_t{2}, B->getDecodeMode());
  EXPECT_EQ(BI, B->getByteInterval());
}

TEST(Unit_CodeBlock, getAddress) {
  auto* BI = ByteInterval::Create(Ctx, nullptr, Addr(10), 10);
  auto* B1 = BI->addCodeBlock(Ctx, 0, 0);
  auto* B2 = BI->addCodeBlock(Ctx, 1, 0);
  auto* B3 = BI->addCodeBlock(Ctx, 10, 0);

  EXPECT_EQ(B1->getAddress(), Addr{10});
  EXPECT_EQ(B2->getAddress(), Addr{11});
  EXPECT_EQ(B3->getAddress(), Addr{20});

  BI->setAddress({});
  EXPECT_EQ(B1->getAddress(), std::optional<Addr>());
  EXPECT_EQ(B2->getAddress(), std::optional<Addr>());
  EXPECT_EQ(B3->getAddress(), std::optional<Addr>());
}

TEST(Unit_CodeBlock, byteVector) {
  std::string Contents = "hello, world!";
  auto* BI = ByteInterval::Create(Ctx, nullptr, std::optional<Addr>(),
                                  Contents.begin(), Contents.end());
  auto* B = BI->addCodeBlock(Ctx, 3, 4);

  auto OriginalIt = Contents.begin() + 3;
  auto NewIt = B->bytes_begin<char>();
  auto OriginalEnd = OriginalIt + 4;
  auto NewEnd = B->bytes_end<char>();

  while (OriginalIt != OriginalEnd && NewIt != NewEnd) {
    EXPECT_EQ(*OriginalIt, *NewIt);
    ++OriginalIt;
    ++NewIt;
  }
  EXPECT_EQ(OriginalIt, OriginalEnd);
  EXPECT_EQ(NewIt, NewEnd);
}

TEST(Unit_Offset, ordering) {
  UUID uuid1;
  UUID uuid2;
  uint64_t disp1(5);
  uint64_t disp2(10);
  Offset offset1(uuid1, disp1);
  Offset offset2(uuid1, disp2);
  Offset offset3(uuid2, disp1);
  Offset offset4(uuid2, disp2);
  EXPECT_TRUE(offset1 < offset2);
  EXPECT_TRUE(offset3 < offset4);
  if (uuid1 < uuid2) {
    EXPECT_TRUE(offset1 < offset3);
    EXPECT_TRUE(offset2 < offset3);
    EXPECT_TRUE(offset1 < offset4);
    EXPECT_TRUE(offset2 < offset4);
  }
  if (uuid2 < uuid1) {
    EXPECT_TRUE(offset1 > offset3);
    EXPECT_TRUE(offset2 > offset3);
    EXPECT_TRUE(offset1 > offset4);
    EXPECT_TRUE(offset2 > offset4);
  }
}

TEST(Unit_Offset, hash) {
  UUID uuid1;
  uint64_t disp1(5);
  uint64_t disp2(10);
  Offset offset1(uuid1, disp1);
  Offset offset2(uuid1, disp2);
  EXPECT_EQ(std::hash<Offset>()(offset1), std::hash<Offset>()(offset1));
  EXPECT_NE(std::hash<Offset>()(offset1), std::hash<Offset>()(offset2));
}

// Note: see Unit_CFG::protobufRoundTrip for Block serialization tests.
