#include <gtirb/Block.hpp>
#include <gtirb/Instruction.hpp>
#include <proto/Block.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

TEST(Unit_Block, ctor) { EXPECT_NO_THROW(Block(EA(), EA())); }

TEST(Unit_Block, getInstructions) {
  Block block{EA(), 0};
  block.getInstructions().emplace_back(EA(123));
  block.getInstructions().emplace_back(EA(456));
  block.getInstructions().emplace_back(EA(789));

  // Instructions were copied by the Block constructor but their contents
  // should be the same.
  const auto& instructions2 = block.getInstructions();
  EXPECT_EQ(instructions2.size(), 3);
  EXPECT_EQ(instructions2[0].getAddress(), EA(123));
  EXPECT_EQ(instructions2[1].getAddress(), EA(456));
  EXPECT_EQ(instructions2[2].getAddress(), EA(789));
}

TEST(Unit_Block, protobufRoundTrip) {
  gtirb::Block result;
  proto::Block message;

  {
    Block original{EA(1), 3};
    original.getInstructions().emplace_back(EA(1));
    original.getInstructions().emplace_back(EA(2));
    original.getInstructions().emplace_back(EA(3));

    original.toProtobuf(&message);
  }
  // original has been destroyed, so UUIDs can be reused
  result.fromProtobuf(message);

  const auto& instructions = result.getInstructions();
  EXPECT_EQ(result.getAddress(), EA(1));
  EXPECT_EQ(result.getSize(), 3);
  EXPECT_EQ(instructions.size(), 3);
  EXPECT_EQ(instructions[0].getAddress(), EA(1));
  EXPECT_EQ(instructions[1].getAddress(), EA(2));
  EXPECT_EQ(instructions[2].getAddress(), EA(3));
}
