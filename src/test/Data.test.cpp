#include <gtest/gtest.h>
#include <proto/Data.pb.h>
#include <gtirb/Data.hpp>

using namespace gtirb;

TEST(Unit_Data, protobufRoundTrip)
{
    Data original(EA(1), 1234);

    gtirb::Data result;
    proto::Data message;
    original.toProtobuf(&message);
    original.setUUID(); // Avoid UUID conflict
    result.fromProtobuf(message);

    EXPECT_EQ(result.getEA(), EA(1));
    EXPECT_EQ(result.getSize(), 1234);
}
