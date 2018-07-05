#include <gtest/gtest.h>
#include <proto/Block.pb.h>
#include <gtirb/Block.hpp>
#include <gtirb/Instruction.hpp>
#include <gtirb/Module.hpp>

using namespace gtirb;

TEST(Unit_Block, ctor)
{
    EXPECT_NO_THROW(Block(EA(), EA()));
}

TEST(Unit_Block, getInstructions)
{
    Block block(EA(), EA(), {Instruction(EA(123)), Instruction(EA(456)), Instruction(EA(789))});

    // Instructions were copied by the Block constructor but their contents
    // should be the same.
    const auto instructions2 = block.getInstructions();
    EXPECT_EQ(instructions2.size(), 3);
    EXPECT_EQ(instructions2[0].getEA(), EA(123));
    EXPECT_EQ(instructions2[1].getEA(), EA(456));
    EXPECT_EQ(instructions2[2].getEA(), EA(789));
}

TEST(Unit_Block, protobufRoundTrip)
{
    Block original(EA(1), EA(4), {Instruction(EA(1)), Instruction(EA(2)), Instruction(EA(3))});

    gtirb::Block result;
    proto::Block message;
    original.toProtobuf(&message);
    result.fromProtobuf(message);

    const auto& instructions = result.getInstructions();
    EXPECT_EQ(result.getStartingAddress(), EA(1));
    EXPECT_EQ(result.getEndingAddress(), EA(4));
    EXPECT_EQ(instructions.size(), 3);
    EXPECT_EQ(instructions[0].getEA(), EA(1));
    EXPECT_EQ(instructions[1].getEA(), EA(2));
    EXPECT_EQ(instructions[2].getEA(), EA(3));
}
