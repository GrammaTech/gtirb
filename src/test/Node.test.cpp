//===- Node.test.cpp --------------------------------------------*- C++ -*-===//
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
#include <gtirb/Node.hpp>
#include <fstream>
#include <gtest/gtest.h>

static gtirb::Context Ctx;

TEST(Unit_Node, ctor_0) { EXPECT_NE(gtirb::Node::Create(Ctx), nullptr); }

TEST(Unit_Node, uniqueUuids) {
  std::vector<gtirb::UUID> Uuids;

  // Create a bunch of UUID's, then make sure we don't have any duplicates.

  for (size_t I = 0; I < 512; ++I) {
    const auto* N = gtirb::Node::Create(Ctx);
    Uuids.push_back(N->getUUID());
  }

  std::sort(std::begin(Uuids), std::end(Uuids));
  const auto end = std::unique(std::begin(Uuids), std::end(Uuids));

  EXPECT_EQ(std::end(Uuids), end) << "Duplicate UUID's were generated.";
}

// TEST(Unit_Node, copyGetsNewUUID) {
//  gtirb::Node *Node = gtirb::Node::Create(Ctx);
//  gtirb::Node Copy(*Node);
//
//  EXPECT_NE(Node->getUUID(), Copy.getUUID());
//  EXPECT_EQ(gtirb::Node::getByUUID(Node->getUUID()), Node);
//  EXPECT_EQ(gtirb::Node::getByUUID(Copy.getUUID()), &Copy);
//}
