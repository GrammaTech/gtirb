#include <gtirb/Context.hpp>
#include <gtirb/Section.hpp>
#include <proto/Section.pb.h>
#include <gtest/gtest.h>
#include <limits>

using namespace gtirb;

static Context Ctx;

TEST(Unit_Section, containsEA) {
  Section *Good = Section::Create(Ctx, "good", Addr{11}, 100);
  EXPECT_FALSE(containsAddress(Good, Addr(10)));
  EXPECT_TRUE(containsAddress(Good, Addr(11)));
  EXPECT_TRUE(containsAddress(Good, Addr(110)));
  EXPECT_FALSE(containsAddress(Good, Addr(111)));

  Section *Big = Section::Create(Ctx, "big", Addr(0), std::numeric_limits<uint64_t>::max());
  EXPECT_TRUE(containsAddress(Big, Addr(0)));
  EXPECT_TRUE(containsAddress(Big, Addr(std::numeric_limits<uint64_t>::max() - 1)));
  // No section contains a bad address
  EXPECT_FALSE(containsAddress(Big, Addr()));

  // Bad section does not contain anything
  Section *Bad = Section::Create(Ctx, "bad", Addr(), std::numeric_limits<uint64_t>::max());
  EXPECT_FALSE(containsAddress(Bad, Addr(0)));
  EXPECT_FALSE(containsAddress(Bad, Addr(std::numeric_limits<uint64_t>::max() - 1)));
  EXPECT_FALSE(containsAddress(Bad, Addr()));
}

TEST(Unit_Section, protobufRoundTrip) {
  Section *Original = Section::Create(Ctx, "name", Addr(1), 1234);

  proto::Section Message;
  Original->toProtobuf(&Message);
  details::ClearUUIDs(Original); // Avoid UUID conflict
  Section *Result = Section::fromProtobuf(Ctx, Message);

  EXPECT_EQ(Result->getName(), "name");
  EXPECT_EQ(Result->getSize(), 1234);
  EXPECT_EQ(Result->getAddress(), Addr(1));
}
