#include <gtirb/Procedure.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/RuntimeError.hpp>
#include <gtirb/NodeValidators.hpp>
#include <gtirb/Instruction.hpp>

using namespace gtirb;

Instruction::Instruction() : Node()
{
    this->addParentValidator(gtirb::NodeValidatorHasParentOfType<gtirb::Procedure>());
}

void Instruction::setEA(gtirb::EA x)
{
    this->ea = x;
}

gtirb::EA Instruction::getEA() const
{
    return this->ea;
}

void Instruction::setIsFallthrough(bool x)
{
    this->isFallthrough = x;
}

bool Instruction::getIsFallthrough() const
{
    return this->isFallthrough;
}

void Instruction::setIsPEI(bool x)
{
    this->isPEI = x;
}

bool Instruction::getIsPEI() const
{
    return this->isPEI;
}

void Instruction::setNumberOfUses(int64_t x)
{
    this->numberOfUses = x;
}

int64_t Instruction::getNumberOfUses() const
{
    return this->numberOfUses;
}
