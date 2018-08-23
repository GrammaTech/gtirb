#include <gtirb/Section.hpp>
#include <proto/Section.pb.h>
#include <gtest/gtest.h>
#include <limits>

using namespace gtirb;

TEST(Unit_Section, equality) {
  Section A{"foo", Addr{2}, 1};
  Section B{"foo", Addr{2}, 1};
  Section C{"bar", Addr{2}, 1};
  Section D{"foo", Addr{2}, 3};
  Section E{"foo", Addr{3}, 1};
  EXPECT_EQ(A, B);
  EXPECT_NE(A, C);
  EXPECT_NE(A, D);
  EXPECT_NE(A, E);
}

TEST(Unit_Section, containsAddr) {
  Section Good{"good", Addr{11}, 100};
  EXPECT_FALSE(containsAddr(Good, Addr(10)));
  EXPECT_TRUE(containsAddr(Good, Addr(11)));
  EXPECT_TRUE(containsAddr(Good, Addr(110)));
  EXPECT_FALSE(containsAddr(Good, Addr(111)));

  Section Big{"big", Addr(0), std::numeric_limits<uint64_t>::max()};
  EXPECT_TRUE(containsAddr(Big, Addr(0)));
  EXPECT_TRUE(
      containsAddr(Big, Addr(std::numeric_limits<uint64_t>::max() - 1)));
}

TEST(Unit_Section, protobufRoundTrip) {
  Section Original("name", Addr(1), 1234);

  gtirb::Section Result;
  proto::Section Message;
  Original.toProtobuf(&Message);
  Original.setUUID(); // Avoid UUID conflict
  Result.fromProtobuf(Message);

  EXPECT_EQ(Result.getName(), "name");
  EXPECT_EQ(Result.getSize(), 1234);
  EXPECT_EQ(Result.getAddress(), Addr(1));
}
