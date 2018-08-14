#include <gtirb/Block.hpp>
#include <proto/Block.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

TEST(Unit_Block, ctor) { EXPECT_NO_THROW(Block(EA(), 0)); }

TEST(Unit_Block, getters) {
  Block B(EA(1), 2, 3);
  EXPECT_EQ(EA(1), B.getAddress());
  EXPECT_EQ(uint64_t{2}, B.getSize());
  EXPECT_EQ(uint64_t{3}, B.getDecodeMode());
}

TEST(Unit_Block, protobufRoundTrip) {
  gtirb::Block Result;
  proto::Block Message;

  {
    Block Original{EA(1), 3, 5};
    Original.toProtobuf(&Message);
  }
  // original has been destroyed, so UUIDs can be reused
  Result.fromProtobuf(Message);

  EXPECT_EQ(Result.getAddress(), EA(1));
  EXPECT_EQ(Result.getSize(), 3);
  EXPECT_EQ(Result.getDecodeMode(), 5);
}
