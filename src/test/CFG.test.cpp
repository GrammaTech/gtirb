#include <gtirb/Block.hpp>
#include <gtirb/CFG.hpp>
#include <gtirb/Context.hpp>
#include <proto/CFG.pb.h>
#include <gtest/gtest.h>

//
#include <boost/uuid/uuid_io.hpp>
//

using namespace gtirb;

static Context Ctx;

TEST(Unit_CFG, ctor_0) { EXPECT_NO_THROW(CFG()); }

TEST(Unit_CFG, addVertex) {
  //  CFG Cfg;
  //  auto Descriptor = add_vertex(Cfg);
  //  EXPECT_EQ(Cfg[Descriptor]->getAddress(), Addr());
  //  EXPECT_EQ(Cfg[Descriptor]->getSize(), 0);
}

TEST(Unit_CFG, addBlock) {
  CFG Cfg;
  auto Descriptor = addBlock(Cfg, Block::Create(Ctx, Addr(1), 2));
  EXPECT_EQ(Cfg[Descriptor]->getAddress(), Addr(1));
  EXPECT_EQ(Cfg[Descriptor]->getSize(), 2);
}

TEST(Unit_CFG, blockIterator) {
  CFG Cfg;
  addBlock(Cfg, Block::Create(Ctx, Addr(1), 2));
  addBlock(Cfg, Block::Create(Ctx, Addr(3), 2));
  addBlock(Cfg, Block::Create(Ctx, Addr(5), 2));

  // Non-const graph produces a regular iterator
  boost::iterator_range<block_iterator> BlockRange = blocks(Cfg);
  EXPECT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 3);
  auto It = BlockRange.begin();
  EXPECT_EQ(It->getAddress(), Addr(1));
  ++It;
  EXPECT_EQ(It->getAddress(), Addr(3));
  ++It;
  EXPECT_EQ(It->getAddress(), Addr(5));
  ++It;
  EXPECT_EQ(It, BlockRange.end());

  // Const graph produces a const iterator
  const CFG& ConstCfg = Cfg;
  boost::iterator_range<const_block_iterator> ConstRange = blocks(ConstCfg);
  EXPECT_EQ(std::distance(ConstRange.begin(), ConstRange.end()), 3);
  auto Cit = ConstRange.begin();
  EXPECT_EQ(Cit->getAddress(), Addr(1));
  ++Cit;
  EXPECT_EQ(Cit->getAddress(), Addr(3));
  ++Cit;
  EXPECT_EQ(Cit->getAddress(), Addr(5));
  ++Cit;
  EXPECT_EQ(Cit, ConstRange.end());
}

TEST(Unit_CFG, edges) {
  CFG Cfg;
  auto B1 = addBlock(Cfg, Block::Create(Ctx, Addr(1), Addr(2)));
  auto B2 = addBlock(Cfg, Block::Create(Ctx, Addr(3), Addr(4)));
  auto B3 = addBlock(Cfg, Block::Create(Ctx, Addr(5), Addr(6)));

  auto E1 = add_edge(B1, B3, Cfg);
  EXPECT_EQ(source(E1.first, Cfg), B1);
  EXPECT_EQ(target(E1.first, Cfg), B3);
  EXPECT_TRUE(E1.second);

  auto E2 = add_edge(B2, B3, Cfg);
  EXPECT_EQ(source(E2.first, Cfg), B2);
  EXPECT_EQ(target(E2.first, Cfg), B3);
  EXPECT_TRUE(E2.second);

  auto E3 = add_edge(B3, B1, Cfg);
  EXPECT_EQ(source(E3.first, Cfg), B3);
  EXPECT_EQ(target(E3.first, Cfg), B1);
  EXPECT_TRUE(E3.second);

  // Parallel edge
  auto E4 = add_edge(B1, B3, Cfg);
  EXPECT_EQ(source(E4.first, Cfg), B1);
  EXPECT_EQ(target(E4.first, Cfg), B3);
  EXPECT_TRUE(E4.second);
}

TEST(Unit_CFG, edgeLabels) {
  CFG Cfg;
  auto B1 = addBlock(Cfg, Block::Create(Ctx, Addr(1), Addr(2)));
  auto B2 = addBlock(Cfg, Block::Create(Ctx, Addr(3), Addr(4)));

  // boolean label
  auto E1 = add_edge(B1, B2, Cfg).first;
  Cfg[E1] = true;
  EXPECT_EQ(std::get<bool>(Cfg[E1]), true);

  // numeric label
  auto E2 = add_edge(B1, B2, Cfg).first;
  Cfg[E2] = uint64_t(5);
  EXPECT_EQ(std::get<uint64_t>(Cfg[E2]), 5);
}

TEST(Unit_CFG, protobufRoundTrip) {
  CFG Result;
  proto::CFG Message;
  UUID Id1, Id2, Id3;

  {
    CFG Original;
    auto B1 = addBlock(Original, Block::Create(Ctx, Addr(1), Addr(2)));
    auto B2 = addBlock(Original, Block::Create(Ctx, Addr(3), Addr(4)));
    auto B3 = addBlock(Original, Block::Create(Ctx, Addr(5), Addr(6)));

    auto E1 = add_edge(B1, B3, Original).first;
    auto E2 = add_edge(B2, B3, Original).first;
    add_edge(B3, B1, Original);
    Original[E1] = true;
    Original[E2] = uint64_t(5);

    Id1 = Original[B1]->getUUID();
    Id2 = Original[B2]->getUUID();
    Id3 = Original[B3]->getUUID();

    Message = toProtobuf(Original);
    details::ClearUUIDs(Original[B1]); // Avoid UUID conflict
    details::ClearUUIDs(Original[B2]); // Avoid UUID conflict
    details::ClearUUIDs(Original[B3]); // Avoid UUID conflict
  }
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(blocks(Result).size(), 3);
  auto It = blocks(Result).begin();
  EXPECT_EQ(It->getUUID(), Id1);
  ++It;
  EXPECT_EQ(It->getUUID(), Id2);
  ++It;
  EXPECT_EQ(It->getUUID(), Id3);

  // Check edges
  EXPECT_TRUE(edge(vertex(0, Result), vertex(2, Result), Result).second);
  EXPECT_TRUE(edge(vertex(1, Result), vertex(2, Result), Result).second);
  EXPECT_TRUE(edge(vertex(2, Result), vertex(0, Result), Result).second);

  // Check nonexistent edges
  EXPECT_FALSE(edge(vertex(0, Result), vertex(1, Result), Result).second);
  EXPECT_FALSE(edge(vertex(1, Result), vertex(0, Result), Result).second);
  EXPECT_FALSE(edge(vertex(2, Result), vertex(1, Result), Result).second);

  // Check labels
  auto E1 = edge(vertex(0, Result), vertex(2, Result), Result).first;
  EXPECT_EQ(std::get<bool>(Result[E1]), true);

  auto E2 = edge(vertex(1, Result), vertex(2, Result), Result).first;
  EXPECT_EQ(std::get<uint64_t>(Result[E2]), 5);
}
