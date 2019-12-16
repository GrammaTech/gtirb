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
#include <gtirb/Context.hpp>
#include <proto/ByteInterval.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

TEST(Unit_ByteInterval, ctor) {
  EXPECT_NE(ByteInterval::Create(Ctx, nullptr, Addr(1), 100), nullptr);
}

TEST(Unit_ByteInterval, gettersSetters) {
  auto BI = ByteInterval::Create(Ctx, nullptr, std::optional<Addr>(), 2);
  EXPECT_EQ(std::optional<Addr>(), BI->getAddress());
  EXPECT_EQ(2, BI->getSize());
  EXPECT_EQ(2, BI->getAllocatedSize());
  EXPECT_EQ(BI->getSection(), nullptr);

  BI->setAddress(Addr(1));
  EXPECT_EQ(std::optional<Addr>(1), BI->getAddress());
  EXPECT_EQ(2, BI->getSize());
  EXPECT_EQ(2, BI->getAllocatedSize());
  EXPECT_EQ(BI->getSection(), nullptr);

  BI->setSize(10);
  EXPECT_EQ(std::optional<Addr>(1), BI->getAddress());
  EXPECT_EQ(10, BI->getSize());
  EXPECT_EQ(2, BI->getAllocatedSize());
  EXPECT_EQ(BI->getSection(), nullptr);

  BI->setAllocatedSize(5);
  EXPECT_EQ(std::optional<Addr>(1), BI->getAddress());
  EXPECT_EQ(10, BI->getSize());
  EXPECT_EQ(5, BI->getAllocatedSize());
  EXPECT_EQ(BI->getSection(), nullptr);

  BI->setSize(1);
  EXPECT_EQ(std::optional<Addr>(1), BI->getAddress());
  EXPECT_EQ(1, BI->getSize());
  EXPECT_EQ(1, BI->getAllocatedSize());
  EXPECT_EQ(BI->getSection(), nullptr);

  BI->setAllocatedSize(20);
  EXPECT_EQ(std::optional<Addr>(1), BI->getAddress());
  EXPECT_EQ(20, BI->getSize());
  EXPECT_EQ(20, BI->getAllocatedSize());
  EXPECT_EQ(BI->getSection(), nullptr);
}

TEST(Unit_ByteInterval, protobufRoundTrip) {
  // test with fixed address
  {
    proto::ByteInterval Message;
    {
      Context InnerCtx;
      ByteInterval* Original =
          ByteInterval::Create(InnerCtx, nullptr, Addr(1), 2);
      Original->toProtobuf(&Message);
    }
    ByteInterval* Result = ByteInterval::fromProtobuf(Ctx, nullptr, Message);

    EXPECT_EQ(Result->getAddress(), std::optional<Addr>(1));
    EXPECT_EQ(Result->getSize(), 2);
    EXPECT_EQ(Result->getSection(), nullptr);
  }

  // test without fixed address
  {
    proto::ByteInterval Message;
    {
      Context InnerCtx;
      ByteInterval* Original =
          ByteInterval::Create(InnerCtx, nullptr, std::optional<Addr>(), 2);
      Original->toProtobuf(&Message);
    }
    ByteInterval* Result = ByteInterval::fromProtobuf(Ctx, nullptr, Message);

    EXPECT_EQ(Result->getAddress(), std::optional<Addr>());
    EXPECT_EQ(Result->getSize(), 2);
    EXPECT_EQ(Result->getSection(), nullptr);
  }

  // test with subobjects
  {
    auto Sym = Symbol::Create(Ctx, nullptr, "test");

    proto::ByteInterval Message;
    {
      Context InnerCtx;
      ByteInterval* Original =
          ByteInterval::Create(InnerCtx, nullptr, Addr(0), 10);
      Original->addCodeBlock(InnerCtx, 3, 1);
      Original->addCodeBlock(InnerCtx, 6, 1);
      Original->addDataBlock(InnerCtx, 6, 1);
      Original->addSymbolicExpression<SymAddrConst>(5, 8, Sym);
      Original->toProtobuf(&Message);
    }
    ByteInterval* Result = ByteInterval::fromProtobuf(Ctx, nullptr, Message);

    EXPECT_EQ(std::distance(Result->blocks_begin(), Result->blocks_end()), 3);
    EXPECT_EQ(
        std::distance(Result->code_blocks_begin(), Result->code_blocks_end()),
        2);
    EXPECT_EQ(
        std::distance(Result->data_blocks_begin(), Result->data_blocks_end()),
        1);
    // only after symbolicExpressionsFromProtobuf will sym exprs be populated
    EXPECT_EQ(std::distance(Result->symbolic_expressions_begin(),
                            Result->symbolic_expressions_end()),
              0);

    EXPECT_EQ(Result->blocks_begin()->getOffset(), 3);
    EXPECT_EQ(std::next(Result->blocks_begin())->getOffset(), 6);
    EXPECT_EQ(std::next(std::next(Result->blocks_begin()))->getOffset(), 6);

    // populate sym exprs now
    Result->symbolicExpressionsFromProtobuf(Ctx, Message);
    EXPECT_EQ(std::distance(Result->symbolic_expressions_begin(),
                            Result->symbolic_expressions_end()),
              1);

    EXPECT_EQ(Result->symbolic_expressions_begin()->first, 5);
    EXPECT_TRUE(std::holds_alternative<SymAddrConst>(
        Result->symbolic_expressions_begin()->second));
    EXPECT_EQ(
        std::get<SymAddrConst>(Result->symbolic_expressions_begin()->second)
            .Offset,
        8);
    EXPECT_EQ(
        std::get<SymAddrConst>(Result->symbolic_expressions_begin()->second)
            .Sym,
        Sym);
  }
}
