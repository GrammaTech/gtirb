#include <gtirb/Context.hpp>
#include <gtirb/Node.hpp>
#include <fstream>
#include <gtest/gtest.h>

static gtirb::Context Ctx;

TEST(Unit_Node, ctor_0) { EXPECT_NO_THROW(gtirb::Node::Create(Ctx)); }

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
