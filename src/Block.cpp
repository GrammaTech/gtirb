#include <gtirb/Block.hpp>
#include <gtirb/Instruction.hpp>
#include <gtirb/Module.hpp>

using namespace gtirb;
class Module;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::Block);

Block::Block(EA startingAddress, EA endingAddress)
    : Node(), startingAddress(startingAddress), endingAddress(endingAddress)
{
}

Block::Block(EA startingAddress, EA endingAddress, std::vector<Instruction>& instructions)
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

const std::vector<Instruction>& Block::getInstructions() const
{
    return this->instructions;
}
