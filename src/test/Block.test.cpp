#include <gtirb/Block.hpp>
#include <proto/Block.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

TEST(Unit_Block, ctor) { EXPECT_NO_THROW(Block(EA(), EA())); }

TEST(Unit_Block, protobufRoundTrip) {
  gtirb::Block result;
  proto::Block message;

  {
    Block original{EA(1), 3};
    original.toProtobuf(&message);
  }
  // original has been destroyed, so UUIDs can be reused
  result.fromProtobuf(message);

  EXPECT_EQ(result.getAddress(), EA(1));
  EXPECT_EQ(result.getSize(), 3);
}
