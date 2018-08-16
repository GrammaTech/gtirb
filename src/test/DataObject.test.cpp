#include <gtirb/DataObject.hpp>
#include <proto/DataObject.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

TEST(Unit_DataObject, getters) {
  DataObject D(Addr(1), 1234);
  EXPECT_EQ(D.getAddress(), Addr(1));
  EXPECT_EQ(D.getSize(), 1234);
}

TEST(Unit_DataObject, protobufRoundTrip) {
  DataObject Original(Addr(1), 1234);

  gtirb::DataObject Result;
  proto::DataObject Message;
  Original.toProtobuf(&Message);
  Original.setUUID(); // Avoid UUID conflict
  Result.fromProtobuf(Message);

  EXPECT_EQ(Result.getAddress(), Addr(1));
  EXPECT_EQ(Result.getSize(), 1234);
}
