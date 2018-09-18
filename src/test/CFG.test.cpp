//===- CFG.test.cpp ---------------------------------------------*- C++ -*-===//
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

TEST(Unit_CFG, addVertex) {
  //  CFG Cfg;
  //  auto Descriptor = add_vertex(Cfg);
  //  EXPECT_EQ(Cfg[Descriptor]->getAddress(), Addr());
  //  EXPECT_EQ(Cfg[Descriptor]->getSize(), 0);
}

TEST(Unit_CFG, addBlock) {
  CFG Cfg;
  auto B = emplaceBlock(Cfg, Ctx, Addr(1), 2);
  EXPECT_EQ(Cfg[B->getVertex()]->getAddress(), Addr(1));
  EXPECT_EQ(Cfg[B->getVertex()]->getSize(), 2);
}

TEST(Unit_CFG, blockIterator) {
  CFG Cfg;
  emplaceBlock(Cfg, Ctx, Addr(1), 2);
  emplaceBlock(Cfg, Ctx, Addr(3), 2);
  emplaceBlock(Cfg, Ctx, Addr(5), 2);

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
  auto B1 = emplaceBlock(Cfg, Ctx, Addr(1), 2);
  auto B2 = emplaceBlock(Cfg, Ctx, Addr(3), 4);
  auto B3 = emplaceBlock(Cfg, Ctx, Addr(5), 6);

  auto E1 = addEdge(B1, B3, Cfg);
  EXPECT_EQ(Cfg[source(E1, Cfg)], B1);
  EXPECT_EQ(Cfg[target(E1, Cfg)], B3);

  auto E2 = addEdge(B2, B3, Cfg);
  EXPECT_EQ(Cfg[source(E2, Cfg)], B2);
  EXPECT_EQ(Cfg[target(E2, Cfg)], B3);

  auto E3 = addEdge(B3, B1, Cfg);
  EXPECT_EQ(Cfg[source(E3, Cfg)], B3);
  EXPECT_EQ(Cfg[target(E3, Cfg)], B1);

  // Parallel edge
  auto E4 = addEdge(B1, B3, Cfg);
  EXPECT_EQ(Cfg[source(E4, Cfg)], B1);
  EXPECT_EQ(Cfg[target(E4, Cfg)], B3);
}

TEST(Unit_CFG, edgeLabels) {
  CFG Cfg;
  auto B1 = emplaceBlock(Cfg, Ctx, Addr(1), 2);
  auto B2 = emplaceBlock(Cfg, Ctx, Addr(3), 4);

  // boolean label
  auto E1 = addEdge(B1, B2, Cfg);
  Cfg[E1] = true;
  EXPECT_EQ(std::get<bool>(Cfg[E1]), true);

  // numeric label
  auto E2 = addEdge(B1, B2, Cfg);
  Cfg[E2] = uint64_t(5);
  EXPECT_EQ(std::get<uint64_t>(Cfg[E2]), 5);

  // parallel edge with different label
  auto E3 = addEdge(B1, B2, Cfg);
  Cfg[E3] = true;
  EXPECT_EQ(std::get<bool>(Cfg[E3]), true);
  EXPECT_EQ(std::get<uint64_t>(Cfg[E2]), 5);
}

TEST(Unit_CFG, protobufRoundTrip) {
  CFG Result;
  proto::CFG Message;
  UUID Id1, Id2, Id3;

  {
    Context InnerCtx;
    CFG Original;
    auto B1 =
        emplaceBlock(Original, InnerCtx, Addr(1), 2, Block::Exit::Branch, 3);
    auto B2 =
        emplaceBlock(Original, InnerCtx, Addr(4), 5, Block::Exit::Call, 6);
    auto B3 =
        emplaceBlock(Original, InnerCtx, Addr(7), 8, Block::Exit::Return, 9);

    auto E1 = addEdge(B1, B3, Original);
    auto E2 = addEdge(B2, B3, Original);
    addEdge(B3, B1, Original);
    Original[E1] = true;
    Original[E2] = uint64_t(5);

    Id1 = Original[B1->getVertex()]->getUUID();
    Id2 = Original[B2->getVertex()]->getUUID();
    Id3 = Original[B3->getVertex()]->getUUID();

    Message = toProtobuf(Original);
  }
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(blocks(Result).size(), 3);
  auto It = blocks(Result).begin();
  EXPECT_EQ(It->getUUID(), Id1);
  EXPECT_EQ(It->getAddress(), Addr(1));
  EXPECT_EQ(It->getSize(), 2);
  EXPECT_EQ(It->getDecodeMode(), 3);
  EXPECT_EQ(It->getExitKind(), Block::Exit::Branch);
  ++It;
  EXPECT_EQ(It->getUUID(), Id2);
  EXPECT_EQ(It->getAddress(), Addr(4));
  EXPECT_EQ(It->getSize(), 5);
  EXPECT_EQ(It->getDecodeMode(), 6);
  EXPECT_EQ(It->getExitKind(), Block::Exit::Call);
  ++It;
  EXPECT_EQ(It->getUUID(), Id3);
  EXPECT_EQ(It->getAddress(), Addr(7));
  EXPECT_EQ(It->getSize(), 8);
  EXPECT_EQ(It->getDecodeMode(), 9);
  EXPECT_EQ(It->getExitKind(), Block::Exit::Return);

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
