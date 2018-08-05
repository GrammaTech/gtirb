#include <gtirb/DataObject.hpp>
#include <proto/DataObject.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

TEST(Unit_DataObject, getters) {
  DataObject d(EA(1), 1234);
  EXPECT_EQ(d.getAddress(), EA(1));
  EXPECT_EQ(d.getSize(), 1234);
}

TEST(Unit_DataObject, protobufRoundTrip) {
  DataObject original(EA(1), 1234);

  gtirb::DataObject result;
  proto::DataObject message;
  original.toProtobuf(&message);
  original.setUUID(); // Avoid UUID conflict
  result.fromProtobuf(message);

  EXPECT_EQ(result.getAddress(), EA(1));
  EXPECT_EQ(result.getSize(), 1234);
}
