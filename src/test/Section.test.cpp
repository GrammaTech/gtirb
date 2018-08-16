#include <gtirb/Context.hpp>
#include <gtirb/Section.hpp>
#include <proto/Section.pb.h>
#include <gtest/gtest.h>
#include <limits>

using namespace gtirb;

static Context Ctx;

TEST(Unit_Section, equality) {
  Section *A = Section::Create(Ctx, "foo", EA{2}, 1);
  Section *B = Section::Create(Ctx, "foo", EA{2}, 1);
  Section *C = Section::Create(Ctx, "bar", EA{2}, 1);
  Section *D = Section::Create(Ctx, "foo", EA{2}, 3);
  Section *E = Section::Create(Ctx, "foo", EA{3}, 1);
  EXPECT_EQ(*A, *B);
  EXPECT_NE(*A, *C);
  EXPECT_NE(*A, *D);
  EXPECT_NE(*A, *E);
}

TEST(Unit_Section, containsEA) {
  Section *Good = Section::Create(Ctx, "good", EA{11}, 100);
  EXPECT_FALSE(containsEA(Good, EA(10)));
  EXPECT_TRUE(containsEA(Good, EA(11)));
  EXPECT_TRUE(containsEA(Good, EA(110)));
  EXPECT_FALSE(containsEA(Good, EA(111)));

  Section *Big = Section::Create(Ctx, "big", EA(0), std::numeric_limits<uint64_t>::max());
  EXPECT_TRUE(containsEA(Big, EA(0)));
  EXPECT_TRUE(containsEA(Big, EA(std::numeric_limits<uint64_t>::max() - 1)));
  // No section contains a bad address
  EXPECT_FALSE(containsEA(Big, EA()));

  // Bad section does not contain anything
  Section *Bad = Section::Create(Ctx, "bad", EA(), std::numeric_limits<uint64_t>::max());
  EXPECT_FALSE(containsEA(Bad, EA(0)));
  EXPECT_FALSE(containsEA(Bad, EA(std::numeric_limits<uint64_t>::max() - 1)));
  EXPECT_FALSE(containsEA(Bad, EA()));
}

TEST(Unit_Section, protobufRoundTrip) {
  Section *Original = Section::Create(Ctx, "name", EA(1), 1234);

  proto::Section Message;
  Original->toProtobuf(&Message);
  Original->setUUID(); // Avoid UUID conflict
  Section *Result = Section::fromProtobuf(Ctx, Message);

  EXPECT_EQ(Result->getName(), "name");
  EXPECT_EQ(Result->getSize(), 1234);
  EXPECT_EQ(Result->getAddress(), EA(1));
}
