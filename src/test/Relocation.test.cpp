#include <gtest/gtest.h>
#include <proto/Relocation.pb.h>
#include <gtirb/Relocation.hpp>

using namespace gtirb;

TEST(Unit_Relocation, protobufRoundTrip) {
  Relocation original{EA(1), "type", "name", 2};

  gtirb::Relocation result;
  proto::Relocation message;
  original.toProtobuf(&message);
  result.fromProtobuf(message);

  EXPECT_EQ(result.ea, EA(1));
  EXPECT_EQ(result.type, "type");
  EXPECT_EQ(result.name, "name");
  EXPECT_EQ(result.offset, 2);
}
