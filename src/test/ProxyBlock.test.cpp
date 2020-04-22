//===- ProxyBlock.test.cpp ---------------------------------------*- C++
//-*-===//
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
#include <gtirb/ProxyBlock.hpp>
#include <gtest/gtest.h>
#include <sstream>

using namespace gtirb;

static Context Ctx;

TEST(Unit_ProxyBlock, noCopyMoveConstructors) {
  EXPECT_FALSE(std::is_copy_constructible_v<ProxyBlock>);
  EXPECT_FALSE(std::is_move_constructible_v<ProxyBlock>);
  EXPECT_FALSE(std::is_copy_assignable_v<ProxyBlock>);
  EXPECT_FALSE(std::is_move_assignable_v<ProxyBlock>);
}

TEST(Unit_ProxyBlock, protobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  std::stringstream ss;
  UUID uuid;
  {
    Context InnerCtx;
    ProxyBlock* Original = ProxyBlock::Create(InnerCtx);
    uuid = Original->getUUID();
    STH::save(*Original, ss);
  }
  ProxyBlock* Result = STH::load<ProxyBlock>(Ctx, ss);

  EXPECT_EQ(uuid, Result->getUUID());
  EXPECT_EQ(Result->getModule(), nullptr);
}
