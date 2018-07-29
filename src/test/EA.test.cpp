#include <gtest/gtest.h>
#include <gtirb/EA.hpp>
#include <sstream>

using namespace gtirb;

TEST(Unit_EA, ctor_0) {
  EXPECT_NO_THROW(EA());
  EXPECT_EQ(EA(constants::BadAddress), EA());
}

TEST(Unit_EA, ctor_1) {
  EXPECT_NO_THROW(EA(2112));

  auto ea = EA(2112);
  EXPECT_EQ(uint64_t(2112), ea.get());
}

TEST(Unit_EA, comparison) {
  auto ea1 = EA(2112);
  auto ea2 = EA(1221);

  EXPECT_GT(ea1, ea2);
  EXPECT_TRUE(ea1 > ea2);

  EXPECT_NE(ea1, ea2);
  EXPECT_TRUE(ea1 != ea2);

  EXPECT_LT(ea2, ea1);
  EXPECT_TRUE(ea2 < ea1);

  EXPECT_FALSE(ea1 == ea2);
}

TEST(Unit_EA, set) {
  auto ea1 = EA(2112);
  auto ea2 = EA();

  ea2.set(uint64_t(2112));

  EXPECT_EQ(ea1, ea2);
  EXPECT_EQ(uint64_t(2112), ea2.get());
}

TEST(Unit_EA, ostream) {
  std::ostringstream os;

  os << 123 << " " << EA(456) << " 789";
  EXPECT_EQ(os.str(), "123 0x1c8 789");
}
