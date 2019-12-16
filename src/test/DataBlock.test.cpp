//===- DataBlock.test.cpp --------------------------------------*- C++ -*-===//
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
#include <gtirb/Context.hpp>
#include <gtirb/DataBlock.hpp>
#include <proto/DataBlock.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

TEST(Unit_DataBlock, getters) {
  auto BI = ByteInterval::Create(Ctx, nullptr, Addr(0), 2);
  auto B = BI->addDataBlock(Ctx, 0, 1);

  EXPECT_EQ(Addr{0}, B->getAddress());
  EXPECT_EQ(uint64_t{1}, B->getSize());
  EXPECT_EQ(BI, B->getByteInterval());
}

TEST(Unit_DataBlock, getAddress) {
  auto BI = ByteInterval::Create(Ctx, nullptr, Addr(10), 10);
  auto B1 = BI->addDataBlock(Ctx, 0, 0);
  auto B2 = BI->addDataBlock(Ctx, 1, 0);
  auto B3 = BI->addDataBlock(Ctx, 10, 0);

  EXPECT_EQ(B1->getAddress(), Addr{10});
  EXPECT_EQ(B2->getAddress(), Addr{11});
  EXPECT_EQ(B3->getAddress(), Addr{20});

  BI->setAddress({});
  EXPECT_EQ(B1->getAddress(), std::optional<Addr>());
  EXPECT_EQ(B2->getAddress(), std::optional<Addr>());
  EXPECT_EQ(B3->getAddress(), std::optional<Addr>());
}

TEST(Unit_DataBlock, protobufRoundTrip) {
  proto::DataBlock Message;
  {
    Context InnerCtx;
    DataBlock* Original = DataBlock::Create(InnerCtx, nullptr, 1234);
    Original->toProtobuf(&Message);
  }
  DataBlock* Result = DataBlock::fromProtobuf(Ctx, nullptr, Message);

  EXPECT_EQ(Result->getSize(), 1234);
  EXPECT_EQ(Result->getByteInterval(), nullptr);
}

TEST(Unit_DataBlock, byteVector) {
  std::string contents = "hello, world!";
  auto BI = ByteInterval::Create(Ctx, nullptr, std::optional<Addr>(),
                                 contents.begin(), contents.end());
  auto B = BI->addDataBlock(Ctx, 3, 4);

  auto originalIt = contents.begin() + 3;
  auto newIt = B->bytes_begin<char>();
  auto originalEnd = originalIt + 4;
  auto newEnd = B->bytes_end<char>();

  while (originalIt != originalEnd && newIt != newEnd) {
    EXPECT_EQ(*originalIt, *newIt);
    ++originalIt;
    ++newIt;
  }
  EXPECT_EQ(originalIt, originalEnd);
  EXPECT_EQ(newIt, newEnd);
}
