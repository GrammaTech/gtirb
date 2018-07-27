#include <gtest/gtest.h>
#include <proto/Section.pb.h>
#include <gtirb/Section.hpp>
#include <gtirb/Utilities.hpp>
#include <limits>

using namespace gtirb;

TEST(Unit_Section, equality) {
  Section a{"foo", EA{2}, 1};
  Section b{"foo", EA{2}, 1};
  Section c{"bar", EA{2}, 1};
  Section d{"foo", EA{2}, 3};
  Section e{"foo", EA{3}, 1};
  EXPECT_EQ(a, b);
  EXPECT_NE(a, c);
  EXPECT_NE(a, d);
  EXPECT_NE(a, e);
}

TEST(Unit_Section, containsEA) {
  Section good{"good", EA{11}, 100};
  EXPECT_FALSE(utilities::containsEA(good, EA(10)));
  EXPECT_TRUE(utilities::containsEA(good, EA(11)));
  EXPECT_TRUE(utilities::containsEA(good, EA(110)));
  EXPECT_FALSE(utilities::containsEA(good, EA(111)));

  Section big{"big", EA(0), std::numeric_limits<uint64_t>::max()};
  EXPECT_TRUE(utilities::containsEA(big, EA(0)));
  EXPECT_TRUE(utilities::containsEA(big, EA(std::numeric_limits<uint64_t>::max() - 1)));
  // No section contains a bad address
  EXPECT_FALSE(utilities::containsEA(big, EA()));

  // Bad section does not contain anything
  Section bad{"bad", EA(), std::numeric_limits<uint64_t>::max()};
  EXPECT_FALSE(utilities::containsEA(bad, EA(0)));
  EXPECT_FALSE(utilities::containsEA(bad, EA(std::numeric_limits<uint64_t>::max() - 1)));
  EXPECT_FALSE(utilities::containsEA(bad, EA()));
}

TEST(Unit_Section, protobufRoundTrip) {
  Section original("name", EA(1), 1234);

  gtirb::Section result;
  proto::Section message;
  original.toProtobuf(&message);
  original.setUUID(); // Avoid UUID conflict
  result.fromProtobuf(message);

  EXPECT_EQ(result.getName(), "name");
  EXPECT_EQ(result.getSize(), 1234);
  EXPECT_EQ(result.getAddress(), EA(1));
}
