#include <gtirb/Node.hpp>
#include <gtirb/NodeRef.hpp>
#include <gtest/gtest.h>

using namespace gtirb;

TEST(Unit_NodeRef, ctorFromUUID) {
  UUID id = Node().getUUID();
  NodeRef<Node> ref(id);

  EXPECT_EQ(ref.getUUID(), id);
}

TEST(Unit_NodeRef, ctorFromNode) {
  Node n;
  NodeRef<Node> ref(n);

  EXPECT_EQ(ref.getUUID(), n.getUUID());
}

TEST(Unit_NodeRef, castToPointer) {
  Node n;
  NodeRef<Node> ref(n);

  EXPECT_EQ(static_cast<Node*>(ref), &n);
}

TEST(Unit_NodeRef, badRefCastsToNullptr) {
  NodeRef<Node> ref(Node().getUUID());

  EXPECT_EQ(static_cast<Node*>(ref), nullptr);
}

TEST(Unit_NodeRef, dereference) {
  Node n;
  NodeRef<Node> ref(n);
  const NodeRef<Node> constRef(n);

  EXPECT_EQ((*ref).getUUID(), n.getUUID());
  EXPECT_EQ((*constRef).getUUID(), n.getUUID());
}

TEST(Unit_NodeRef, arrowOperator) {
  Node n;
  NodeRef<Node> ref(n);
  const NodeRef<Node> constRef(n);

  EXPECT_EQ(ref->getUUID(), n.getUUID());
  EXPECT_EQ(constRef->getUUID(), n.getUUID());
}
