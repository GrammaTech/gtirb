//===- NodeRef.test.cpp -----------------------------------------*- C++ -*-===//
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
#include <gtirb/NodeRef.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

TEST(Unit_NodeRef, ctorFromUUID) {
  UUID Id = Node::Create(Ctx)->getUUID();
  NodeRef<Node> Ref(Id);

  EXPECT_EQ(Ref.getUUID(), Id);
}

TEST(Unit_NodeRef, ctorFromNode) {
  Node* N = Node::Create(Ctx);
  NodeRef<Node> Ref(N);

  EXPECT_EQ(Ref.getUUID(), N->getUUID());
}
