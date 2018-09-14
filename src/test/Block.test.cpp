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
#include <gtirb/Block.hpp>
#include <gtirb/Context.hpp>
#include <proto/Block.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

TEST(Unit_Block, ctor) { EXPECT_NO_THROW(Block::Create(Ctx, Addr(), 0)); }

TEST(Unit_Block, getters) {
  Block* B = Block::Create(Ctx, Addr(1), 2, 3);
  EXPECT_EQ(Addr(1), B->getAddress());
  EXPECT_EQ(uint64_t{2}, B->getSize());
  EXPECT_EQ(uint64_t{3}, B->getDecodeMode());
}

TEST(Unit_Block, protobufRoundTrip) {
  proto::Block Message;
  {
    Context InnerCtx;
    Block* Original = Block::Create(InnerCtx, Addr(1), 3, 5);
    Original->toProtobuf(&Message);
  }
  Block* Result = Block::fromProtobuf(Ctx, Message);

  EXPECT_EQ(Result->getAddress(), Addr(1));
  EXPECT_EQ(Result->getSize(), 3);
  EXPECT_EQ(Result->getDecodeMode(), 5);
}
