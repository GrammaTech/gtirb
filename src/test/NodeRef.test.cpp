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
  Node *N = Node::Create(Ctx);
  NodeRef<Node> Ref(N);

  EXPECT_EQ(Ref.getUUID(), N->getUUID());
}
