#include <gtirb/Block.hpp>
#include <proto/Block.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

TEST(Unit_Block, ctor) { EXPECT_NO_THROW(Block(EA(), 0)); }

TEST(Unit_Block, getters) {
  Block b(EA(1), 2, 3);
  EXPECT_EQ(EA(1), b.getAddress());
  EXPECT_EQ(uint64_t{2}, b.getSize());
  EXPECT_EQ(uint64_t{3}, b.getDecodeMode());
}

TEST(Unit_Block, protobufRoundTrip) {
  gtirb::Block result;
  proto::Block message;

  {
    Block original{EA(1), 3, 5};
    original.toProtobuf(&message);
  }
  // original has been destroyed, so UUIDs can be reused
  result.fromProtobuf(message);

  EXPECT_EQ(result.getAddress(), EA(1));
  EXPECT_EQ(result.getSize(), 3);
  EXPECT_EQ(result.getDecodeMode(), 5);
}
