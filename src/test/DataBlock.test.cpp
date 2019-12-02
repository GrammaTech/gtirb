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
#include <gtirb/Context.hpp>
#include <gtirb/DataBlock.hpp>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

TEST(Unit_DataObject, getters) {
  DataBlock* D = DataBlock::Create(Ctx, Addr(1), 1234);
  EXPECT_EQ(D->getAddress(), Addr(1));
  EXPECT_EQ(D->getSize(), 1234);
}

TEST(Unit_DataObject, protobufRoundTrip) {
  proto::DataBlock Message;
  {
    Context InnerCtx;
    DataBlock* Original = DataBlock::Create(InnerCtx, Addr(1), 1234);
    Original->toProtobuf(&Message);
  }
  DataBlock* Result = DataBlock::fromProtobuf(Ctx, Message);

  EXPECT_EQ(Result->getAddress(), Addr(1));
  EXPECT_EQ(Result->getSize(), 1234);
}
