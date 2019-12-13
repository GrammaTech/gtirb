//===- Section.test.cpp -----------------------------------------*- C++ -*-===//
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
#include <gtirb/Section.hpp>
#include <proto/Section.pb.h>
#include <gtest/gtest.h>
#include <limits>

using namespace gtirb;

static Context Ctx;

TEST(Unit_Section, getAddress) {
  using OAddr = std::optional<Addr>;
  using OSize = std::optional<uint64_t>;

  auto S = Section::Create(Ctx, nullptr, "test");
  EXPECT_EQ(S->getAddress(), OAddr());
  EXPECT_EQ(S->getSize(), OSize(0));

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

TEST(Unit_Section, protobufRoundTrip) {
  proto::Section Message;

  {
    Context InnerCtx;
    Section* Original = Section::Create(InnerCtx, nullptr, "name");
    Original->toProtobuf(&Message);
  }
  Section* Result = Section::fromProtobuf(Ctx, nullptr, Message);

  EXPECT_EQ(Result->getName(), "name");
}
