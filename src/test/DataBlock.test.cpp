//===- DataBlock.test.cpp --------------------------------------*- C++ -*-===//
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
#include <gtirb/ByteInterval.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/DataBlock.hpp>
#include <gtirb/proto/DataBlock.pb.h>
#include <gtest/gtest.h>
#include <sstream>

using namespace gtirb;

static Context Ctx;

TEST(Unit_DataBlock, noCopyMoveConstructors) {
  EXPECT_FALSE(std::is_copy_constructible_v<DataBlock>);
  EXPECT_FALSE(std::is_move_constructible_v<DataBlock>);
  EXPECT_FALSE(std::is_copy_assignable_v<DataBlock>);
  EXPECT_FALSE(std::is_move_assignable_v<DataBlock>);
}

TEST(Unit_DataBlock, getters) {
  auto* BI = ByteInterval::Create(Ctx, Addr(0), 2);
  auto* B = BI->addBlock<DataBlock>(Ctx, 0, 1);

  EXPECT_EQ(Addr{0}, B->getAddress());
  EXPECT_EQ(uint64_t{1}, B->getSize());
  EXPECT_EQ(BI, B->getByteInterval());
}

TEST(Unit_DataBlock, getAddress) {
  auto* BI = ByteInterval::Create(Ctx, Addr(10), 10);
  auto* B1 = BI->addBlock<DataBlock>(Ctx, 0, 0);
  auto* B2 = BI->addBlock<DataBlock>(Ctx, 1, 0);
  auto* B3 = BI->addBlock<DataBlock>(Ctx, 10, 0);

  EXPECT_EQ(B1->getAddress(), Addr{10});
  EXPECT_EQ(B2->getAddress(), Addr{11});
  EXPECT_EQ(B3->getAddress(), Addr{20});

  BI->setAddress({});
  EXPECT_EQ(B1->getAddress(), std::optional<Addr>());
  EXPECT_EQ(B2->getAddress(), std::optional<Addr>());
  EXPECT_EQ(B3->getAddress(), std::optional<Addr>());
}

TEST(Unit_DataBlock, protobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  std::stringstream ss;
  {
    Context InnerCtx;
    DataBlock* Original = DataBlock::Create(InnerCtx, 1234);
    STH::save(*Original, ss);
  }
  DataBlock* Result = STH::load<DataBlock>(Ctx, ss);

  EXPECT_EQ(Result->getSize(), 1234);
  EXPECT_EQ(Result->getByteInterval(), nullptr);
}

TEST(Unit_DataBlock, byteVector) {
  std::string Contents = "hello, world!";
  auto* BI = ByteInterval::Create(Ctx, std::optional<Addr>(), Contents.begin(),
                                  Contents.end());
  auto* B = BI->addBlock<DataBlock>(Ctx, 3, 4);

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
