#include <gtirb/Block.hpp>
#include <gtirb/Instruction.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::Block);

Block::Block(EA startingAddress, EA endingAddress)
    : Node(), startingAddress(startingAddress), endingAddress(endingAddress)
{
}

Block::Block(EA startingAddress, EA endingAddress, std::vector<Instruction>&& instructions)
    : Node(),
      startingAddress(startingAddress),
      endingAddress(endingAddress),
      instructions(instructions)
{
}

EA Block::getStartingAddress() const
{
    return this->startingAddress;
}
EA Block::getEndingAddress() const
{
    return this->endingAddress;
}

std::vector<Instruction>& Block::getInstructions()
{
    return this->instructions;
}

const std::vector<Instruction>& Block::getInstructions() const
{
    return this->instructions;
}
