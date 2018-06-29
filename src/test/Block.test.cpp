#include <gtest/gtest.h>
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
