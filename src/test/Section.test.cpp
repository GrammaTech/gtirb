#include <gtirb/Context.hpp>
#include <gtirb/Section.hpp>
#include <proto/Section.pb.h>
#include <gtest/gtest.h>
#include <limits>

using namespace gtirb;

static Context Ctx;

TEST(Unit_Section, containsAddr) {
  Section& Good = *Section::Create(Ctx, "good", Addr{11}, 100);
  EXPECT_FALSE(containsAddr(Good, Addr(10)));
  EXPECT_TRUE(containsAddr(Good, Addr(11)));
  EXPECT_TRUE(containsAddr(Good, Addr(110)));
  EXPECT_FALSE(containsAddr(Good, Addr(111)));

  Section& Big = *Section::Create(Ctx, "big", Addr(0),
                                  std::numeric_limits<uint64_t>::max());
  EXPECT_TRUE(containsAddr(Big, Addr(0)));
  EXPECT_TRUE(
      containsAddr(Big, Addr(std::numeric_limits<uint64_t>::max() - 1)));
}

TEST(Unit_Section, protobufRoundTrip) {
  proto::Section Message;

  {
    Context InnerCtx;
    Section* Original = Section::Create(InnerCtx, "name", Addr(1), 1234);
    Original->toProtobuf(&Message);
  }
  Section* Result = Section::fromProtobuf(Ctx, Message);

  EXPECT_EQ(Result->getName(), "name");
  EXPECT_EQ(Result->getSize(), 1234);
  EXPECT_EQ(Result->getAddress(), Addr(1));
}
