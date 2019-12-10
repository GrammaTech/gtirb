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

TEST(Unit_Block, ctor) {
  EXPECT_NE(CodeBlock::Create(Ctx, nullptr, 0), nullptr);
}

TEST(Unit_Block, getters) {
  auto BI = ByteInterval::Create(Ctx, nullptr, Addr(0), 2);
  auto B = BI->addCodeBlock(Ctx, 0, 1, 2);

  EXPECT_EQ(Addr{0}, B->getAddress());
  EXPECT_EQ(uint64_t{1}, B->getSize());
  EXPECT_EQ(uint64_t{2}, B->getDecodeMode());
  EXPECT_EQ(BI, B->getByteInterval());
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
