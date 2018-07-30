#include <gtirb/CFG.hpp>
#include <gtirb/EA.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Utilities.hpp>
#include <gtest/gtest.h>
#include <memory>

using namespace gtirb;

TEST(Unit_Utilities, ByteArray8To16) {
  const std::vector<uint8_t> input{2, 1, 1, 2, 0, 1};
  const std::vector<uint16_t> expectedOutput{258, 513, 256};
  ASSERT_NO_THROW(gtirb::utilities::ByteArray8To16(input));

  const auto output = gtirb::utilities::ByteArray8To16(input);

  EXPECT_EQ(expectedOutput[0], output[0]);
  EXPECT_EQ(expectedOutput[1], output[1]);
  EXPECT_EQ(expectedOutput[2], output[2]);
}

TEST(Unit_Utilities, ByteArray8To16Swap) {
  const std::vector<uint8_t> input{2, 1, 1, 2, 0, 1};
  const std::vector<uint16_t> expectedOutput{513, 258, 1};
  ASSERT_NO_THROW(gtirb::utilities::ByteArray8To16(input));

  const auto output = gtirb::utilities::ByteArray8To16(input, true);

  EXPECT_EQ(expectedOutput[0], output[0]);
  EXPECT_EQ(expectedOutput[1], output[1]);
  EXPECT_EQ(expectedOutput[2], output[2]);
}

TEST(Unit_Utilities, ByteArray8To16Error) {
  const std::vector<uint8_t> input{2, 1, 1, 2, 0, 1, 0};
  EXPECT_THROW(gtirb::utilities::ByteArray8To16(input), std::range_error);
}
