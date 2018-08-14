#include <gtirb/Section.hpp>
#include <proto/Section.pb.h>
#include <gtest/gtest.h>
#include <limits>

using namespace gtirb;

TEST(Unit_Section, equality) {
  Section A{"foo", EA{2}, 1};
  Section B{"foo", EA{2}, 1};
  Section C{"bar", EA{2}, 1};
  Section D{"foo", EA{2}, 3};
  Section E{"foo", EA{3}, 1};
  EXPECT_EQ(A, B);
  EXPECT_NE(A, C);
  EXPECT_NE(A, D);
  EXPECT_NE(A, E);
}

TEST(Unit_Section, containsEA) {
  Section Good{"good", EA{11}, 100};
  EXPECT_FALSE(containsEA(Good, EA(10)));
  EXPECT_TRUE(containsEA(Good, EA(11)));
  EXPECT_TRUE(containsEA(Good, EA(110)));
  EXPECT_FALSE(containsEA(Good, EA(111)));

  Section Big{"big", EA(0), std::numeric_limits<uint64_t>::max()};
  EXPECT_TRUE(containsEA(Big, EA(0)));
  EXPECT_TRUE(containsEA(Big, EA(std::numeric_limits<uint64_t>::max() - 1)));
  // No section contains a bad address
  EXPECT_FALSE(containsEA(Big, EA()));

  // Bad section does not contain anything
  Section Bad{"bad", EA(), std::numeric_limits<uint64_t>::max()};
  EXPECT_FALSE(containsEA(Bad, EA(0)));
  EXPECT_FALSE(containsEA(Bad, EA(std::numeric_limits<uint64_t>::max() - 1)));
  EXPECT_FALSE(containsEA(Bad, EA()));
}

TEST(Unit_Section, protobufRoundTrip) {
  Section Original("name", EA(1), 1234);

  gtirb::Section Result;
  proto::Section Message;
  Original.toProtobuf(&Message);
  Original.setUUID(); // Avoid UUID conflict
  Result.fromProtobuf(Message);

  EXPECT_EQ(Result.getName(), "name");
  EXPECT_EQ(Result.getSize(), 1234);
  EXPECT_EQ(Result.getAddress(), EA(1));
}
