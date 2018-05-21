#include <gtest/gtest.h>
#include <gtirb/Block.hpp>
#include <gtirb/Instruction.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/NodeStructureError.hpp>

using namespace gtirb;

TEST(Unit_Block, ctor)
{
    EXPECT_NO_THROW(Block(EA(), EA()));
}

TEST(Unit_Block, validParent)
{
    auto module = std::make_unique<Module>();
    auto child = std::make_unique<Block>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TEST(Unit_Block, invalidParent)
{
    auto module = std::make_unique<Node>();
    auto child = std::make_unique<Block>();
    EXPECT_FALSE(child->getIsValidParent(module.get()));
    EXPECT_THROW(module->push_back(std::move(child)), NodeStructureError);
}

TEST(Unit_Block, getInstructions)
{
    std::vector<Instruction> instructions{Instruction(EA(123)), Instruction(EA(456)),
                                          Instruction(EA(789))};
    Block block(EA(), EA(), instructions);

    // Instructions were copied by the Block constructor but their contents
    // should be the same.
    const auto instruction_ptrs = block.getInstructions();
    EXPECT_EQ(instruction_ptrs.size(), 3);
    EXPECT_EQ(instruction_ptrs[0]->getEA(), EA(123));
    EXPECT_EQ(instruction_ptrs[1]->getEA(), EA(456));
    EXPECT_EQ(instruction_ptrs[2]->getEA(), EA(789));
}
