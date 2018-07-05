#include <gtest/gtest.h>
#include <proto/Region.pb.h>
#include <gtirb/Region.hpp>
#include <gtirb/RegionSet.hpp>
#include <memory>

using namespace gtirb;

TEST(Unit_Region, ctor_0)
{
    EXPECT_NO_THROW(gtirb::Region());
}

TEST(Unit_Region, addEA)
{
    auto region = std::make_unique<gtirb::Region>();
    EXPECT_NO_THROW(region->getEAs());

    auto eaSet = region->getEAs();
    EXPECT_TRUE(eaSet.empty());

    EXPECT_NO_THROW(region->getEAs().insert(gtirb::EA{2112}));
    EXPECT_NO_THROW(eaSet = region->getEAs());
    EXPECT_FALSE(eaSet.empty());
}

TEST(Unit_Region, protobufRoundTrip)
{
    Region original;
    original.getEAs() = {EA(1), EA(2)};

    gtirb::Region result;
    proto::Region message;
    original.toProtobuf(&message);
    result.fromProtobuf(message);

    EXPECT_EQ(result.getEAs(), std::set<EA>({EA(1), EA(2)}));
}
