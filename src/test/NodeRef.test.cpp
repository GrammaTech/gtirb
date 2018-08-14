#include <gtirb/Node.hpp>
#include <gtirb/NodeRef.hpp>
#include <gtest/gtest.h>

using namespace gtirb;

TEST(Unit_NodeRef, ctorFromUUID) {
  UUID Id = Node().getUUID();
  NodeRef<Node> Ref(Id);

  EXPECT_EQ(Ref.getUUID(), Id);
}

TEST(Unit_NodeRef, ctorFromNode) {
  Node N;
  NodeRef<Node> Ref(N);

  EXPECT_EQ(Ref.getUUID(), N.getUUID());
}

TEST(Unit_NodeRef, castToPointer) {
  Node N;
  NodeRef<Node> Ref(N);

  EXPECT_EQ(static_cast<Node*>(Ref), &N);
}

TEST(Unit_NodeRef, badRefCastsToNullptr) {
  NodeRef<Node> Ref(Node().getUUID());

  EXPECT_EQ(static_cast<Node*>(Ref), nullptr);
}

TEST(Unit_NodeRef, deReference) {
  Node N;
  NodeRef<Node> Ref(N);
  const NodeRef<Node> ConstRef(N);

  EXPECT_EQ((*Ref).getUUID(), N.getUUID());
  EXPECT_EQ((*ConstRef).getUUID(), N.getUUID());
}

TEST(Unit_NodeRef, arrowOperator) {
  Node N;
  NodeRef<Node> Ref(N);
  const NodeRef<Node> ConstRef(N);

  EXPECT_EQ(Ref->getUUID(), N.getUUID());
  EXPECT_EQ(ConstRef->getUUID(), N.getUUID());
}
