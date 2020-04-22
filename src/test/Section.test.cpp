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
