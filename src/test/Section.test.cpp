#include <gtest/gtest.h>
#include <proto/Section.pb.h>
#include <gtirb/Section.hpp>
#include <limits>

using namespace gtirb;

TEST(Unit_Section, equality)
{
    Section a{"foo", 1, EA{2}};
    Section b{"foo", 1, EA{2}};
    Section c{"bar", 1, EA{2}};
    Section d{"foo", 3, EA{2}};
    Section e{"foo", 1, EA{3}};
    EXPECT_EQ(a, b);
    EXPECT_NE(a, c);
    EXPECT_NE(a, d);
    EXPECT_NE(a, e);
}

TEST(Unit_Section, addressLimit)
{
    Section good{"foo", 100, EA{11}};
    EXPECT_EQ(good.addressLimit(), EA(111));

    Section bad{"foo", 100, EA{}};
    EXPECT_EQ(bad.addressLimit(), EA());
}

TEST(Unit_Section, contains)
{
    Section good{"good", 100, EA{11}};
    EXPECT_FALSE(good.contains(EA(10)));
    EXPECT_TRUE(good.contains(EA(11)));
    EXPECT_TRUE(good.contains(EA(110)));
    EXPECT_FALSE(good.contains(EA(111)));

    Section big{"big", std::numeric_limits<uint64_t>::max(), EA(0)};
    EXPECT_TRUE(big.contains(EA(0)));
    EXPECT_TRUE(big.contains(EA(std::numeric_limits<uint64_t>::max() - 1)));
    // No section contains a bad address
    EXPECT_FALSE(big.contains(EA()));

    // Bad section does not contain anything
    Section bad{"bad", std::numeric_limits<uint64_t>::max(), EA()};
    EXPECT_FALSE(bad.contains(EA(0)));
    EXPECT_FALSE(bad.contains(EA(std::numeric_limits<uint64_t>::max() - 1)));
    EXPECT_FALSE(bad.contains(EA()));
}

TEST(Unit_Section, protobufRoundTrip)
{
    Section original("name", 1234, EA(1));

    gtirb::Section result;
    proto::Section message;
    original.toProtobuf(&message);
    original.setUUID(); // Avoid UUID conflict
    result.fromProtobuf(message);

    EXPECT_EQ(result.getName(), "name");
    EXPECT_EQ(result.getSize(), 1234);
    EXPECT_EQ(result.getStartingAddress(), EA(1));
}
