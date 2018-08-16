#include <gtirb/Context.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/NodeRef.hpp>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

TEST(Unit_NodeRef, ctorFromUUID) {
  UUID Id = Node::Create(Ctx)->getUUID();
  NodeRef<Node> Ref(Id);

  EXPECT_EQ(Ref.getUUID(), Id);
}

TEST(Unit_NodeRef, ctorFromNode) {
  Node *N = Node::Create(Ctx);
  NodeRef<Node> Ref(N);

  EXPECT_EQ(Ref.getUUID(), N->getUUID());
}

TEST(Unit_NodeRef, castToPointer) {
  Node *N = Node::Create(Ctx);
  NodeRef<Node> Ref(N);

  EXPECT_EQ(static_cast<Node*>(Ref), N);
}

TEST(Unit_NodeRef, badRefCastsToNullptr) {
  NodeRef<Node> Ref(Node::Create(Ctx)->getUUID());

  EXPECT_EQ(static_cast<Node*>(Ref), nullptr);
}

TEST(Unit_NodeRef, deReference) {
  Node *N = Node::Create(Ctx);
  NodeRef<Node> Ref(N);
  const NodeRef<Node> ConstRef(N);

  EXPECT_EQ((*Ref).getUUID(), N->getUUID());
  EXPECT_EQ((*ConstRef).getUUID(), N->getUUID());
}

TEST(Unit_NodeRef, arrowOperator) {
  Node *N = Node::Create(Ctx);
  NodeRef<Node> Ref(N);
  const NodeRef<Node> ConstRef(N);

  EXPECT_EQ(Ref->getUUID(), N->getUUID());
  EXPECT_EQ(ConstRef->getUUID(), N->getUUID());
}
