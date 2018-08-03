#include <gtirb/Block.hpp>
#include <proto/Block.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

TEST(Unit_Block, ctor) { EXPECT_NO_THROW(Block(EA(), EA())); }

TEST(Unit_Block, getDecodeMode) {
  Block b(EA(), 2, 3);
  EXPECT_NO_THROW(b.getDecodeMode());
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
