#include <gtirb/Block.hpp>
#include <gtirb/Instruction.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/NodeUtilities.hpp>
#include <gtirb/NodeValidators.hpp>

using namespace gtirb;
class Module;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::Block);

Block::Block() : Block(EA(), EA())
{
}

Block::Block(EA startingAddress, EA endingAddress)
    : Node(), startingAddress(startingAddress), endingAddress(endingAddress)
{
    this->addParentValidator(NodeValidatorHasParentOfType<Module>);
}

Block::Block(EA startingAddress, EA endingAddress, std::vector<Instruction>& instructions)
    : Block(startingAddress, endingAddress)
{
    for(const auto i : instructions)
    {
        auto ptr = std::make_unique<Instruction>();
        *ptr = i;
        this->push_back(std::move(ptr));
    }
}

EA Block::getStartingAddress() const
{
    return this->startingAddress;
}
EA Block::getEndingAddress() const
{
    return this->endingAddress;
}

std::vector<const Instruction*> Block::getInstructions() const
{
    return GetChildrenOfType<const Instruction>(this);
}
