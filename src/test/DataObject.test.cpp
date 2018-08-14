#include <gtirb/DataObject.hpp>
#include <proto/DataObject.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

TEST(Unit_DataObject, getters) {
  DataObject D(EA(1), 1234);
  EXPECT_EQ(D.getAddress(), EA(1));
  EXPECT_EQ(D.getSize(), 1234);
}

TEST(Unit_DataObject, protobufRoundTrip) {
  DataObject Original(EA(1), 1234);

  gtirb::DataObject Result;
  proto::DataObject Message;
  Original.toProtobuf(&Message);
  Original.setUUID(); // Avoid UUID conflict
  Result.fromProtobuf(Message);

  EXPECT_EQ(Result.getAddress(), EA(1));
  EXPECT_EQ(Result.getSize(), 1234);
}
