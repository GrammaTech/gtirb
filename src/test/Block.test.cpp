#include <gtirb/Block.hpp>
#include <gtirb/Context.hpp>
#include <proto/Block.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

TEST(Unit_Block, ctor) { EXPECT_NO_THROW(Block::Create(Ctx, Addr(), 0)); }

TEST(Unit_Block, getters) {
  Block *B = Block::Create(Ctx, Addr(1), 2, 3);
  EXPECT_EQ(Addr(1), B->getAddress());
  EXPECT_EQ(uint64_t{2}, B->getSize());
  EXPECT_EQ(uint64_t{3}, B->getDecodeMode());
}

TEST(Unit_Block, protobufRoundTrip) {
  proto::Block Message;
  {
    Block *Original = Block::Create(Ctx, Addr(1), 3, 5);
    Original->toProtobuf(&Message);
    details::ClearUUIDs(Original); // Avoid UUID conflict
  }
  Block *Result = Block::fromProtobuf(Ctx, Message);

  EXPECT_EQ(Result->getAddress(), Addr(1));
  EXPECT_EQ(Result->getSize(), 3);
  EXPECT_EQ(Result->getDecodeMode(), 5);
}
