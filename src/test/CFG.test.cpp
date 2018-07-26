#include <gtest/gtest.h>
#include <proto/CFG.pb.h>
#include <gtirb/Block.hpp>
#include <gtirb/CFG.hpp>

//
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
//

using namespace gtirb;

TEST(Unit_CFG, ctor_0) { EXPECT_NO_THROW(CFG()); }

TEST(Unit_CFG, addVertex) {
  CFG cfg;
  auto descriptor = add_vertex(cfg);
  EXPECT_EQ(cfg[descriptor].getStartingAddress(), EA());
  EXPECT_EQ(cfg[descriptor].getEndingAddress(), EA());
  EXPECT_TRUE(cfg[descriptor].getInstructions().empty());
}

TEST(Unit_CFG, addBlock) {
  CFG cfg;
  auto descriptor = addBlock(cfg, Block(EA(1), EA(2)));
  EXPECT_EQ(cfg[descriptor].getStartingAddress(), EA(1));
  EXPECT_EQ(cfg[descriptor].getEndingAddress(), EA(2));
  EXPECT_TRUE(cfg[descriptor].getInstructions().empty());
}

TEST(Unit_CFG, blockIterator) {
  CFG cfg;
  addBlock(cfg, Block(EA(1), EA(2)));
  addBlock(cfg, Block(EA(3), EA(4)));
  addBlock(cfg, Block(EA(5), EA(6)));

  // Non-const graph produces a regular iterator
  boost::iterator_range<block_iterator> blockRange = blocks(cfg);
  EXPECT_EQ(std::distance(blockRange.begin(), blockRange.end()), 3);
  auto it = blockRange.begin();
  EXPECT_EQ(it->getStartingAddress(), EA(1));
  ++it;
  EXPECT_EQ(it->getStartingAddress(), EA(3));
  ++it;
  EXPECT_EQ(it->getStartingAddress(), EA(5));
  ++it;
  EXPECT_EQ(it, blockRange.end());

  // Const graph produces a const iterator
  const CFG& const_cfg = cfg;
  boost::iterator_range<const_block_iterator> constRange = blocks(const_cfg);
  EXPECT_EQ(std::distance(constRange.begin(), constRange.end()), 3);
  auto cit = constRange.begin();
  EXPECT_EQ(cit->getStartingAddress(), EA(1));
  ++cit;
  EXPECT_EQ(cit->getStartingAddress(), EA(3));
  ++cit;
  EXPECT_EQ(cit->getStartingAddress(), EA(5));
  ++cit;
  EXPECT_EQ(cit, constRange.end());
}

TEST(Unit_CFG, edges) {
  CFG cfg;
  auto b1 = addBlock(cfg, Block(EA(1), EA(2)));
  auto b2 = addBlock(cfg, Block(EA(3), EA(4)));
  auto b3 = addBlock(cfg, Block(EA(5), EA(6)));

  auto e1 = add_edge(b1, b3, cfg);
  EXPECT_EQ(source(e1.first, cfg), b1);
  EXPECT_EQ(target(e1.first, cfg), b3);
  EXPECT_TRUE(e1.second);

  auto e2 = add_edge(b2, b3, cfg);
  EXPECT_EQ(source(e2.first, cfg), b2);
  EXPECT_EQ(target(e2.first, cfg), b3);
  EXPECT_TRUE(e2.second);

  auto e3 = add_edge(b3, b1, cfg);
  EXPECT_EQ(source(e3.first, cfg), b3);
  EXPECT_EQ(target(e3.first, cfg), b1);
  EXPECT_TRUE(e3.second);

  // Duplicate edge
  auto e4 = add_edge(b1, b3, cfg);
  EXPECT_EQ(source(e4.first, cfg), b1);
  EXPECT_EQ(target(e4.first, cfg), b3);
  EXPECT_FALSE(e4.second);
}

TEST(Unit_CFG, protobufRoundTrip) {
  CFG result;
  proto::CFG message;
  UUID id1, id2, id3;

  {
    CFG original;
    auto b1 = addBlock(original, Block(EA(1), EA(2)));
    auto b2 = addBlock(original, Block(EA(3), EA(4)));
    auto b3 = addBlock(original, Block(EA(5), EA(6)));

    add_edge(b1, b3, original);
    add_edge(b2, b3, original);
    add_edge(b3, b1, original);

    id1 = original[b1].getUUID();
    id2 = original[b2].getUUID();
    id3 = original[b3].getUUID();

    message = toProtobuf(original);
  }
  // original has been destroyed, so UUIDs can be reused
  fromProtobuf(result, message);

  EXPECT_EQ(blocks(result).size(), 3);
  auto it = blocks(result).begin();
  EXPECT_EQ(it->getUUID(), id1);
  ++it;
  EXPECT_EQ(it->getUUID(), id2);
  ++it;
  EXPECT_EQ(it->getUUID(), id3);

  // Check edges
  EXPECT_TRUE(edge(vertex(0, result), vertex(2, result), result).second);
  EXPECT_TRUE(edge(vertex(1, result), vertex(2, result), result).second);
  EXPECT_TRUE(edge(vertex(2, result), vertex(0, result), result).second);

  // Check nonexistent edges
  EXPECT_FALSE(edge(vertex(0, result), vertex(1, result), result).second);
  EXPECT_FALSE(edge(vertex(1, result), vertex(0, result), result).second);
  EXPECT_FALSE(edge(vertex(2, result), vertex(1, result), result).second);
}
