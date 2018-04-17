#include <gtirb/Procedure.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/RuntimeError.hpp>
#include <gtirb/NodeValidators.hpp>
#include <gtirb/Instruction.hpp>

using namespace gtirb;

Procedure::Procedure() : Node()
{
    this->addParentValidator(gtirb::NodeValidatorHasParentOfType<gtirb::Module>());
}

std::set<gtirb::EA>* Procedure::getPLTEntries()
{
    return &this->pltEntries;
}

const std::set<gtirb::EA>* const Procedure::getPLTEntries() const
{
    return &this->pltEntries;
}

Instruction* Procedure::getOrCreateInstruction()
{
    return gtirb::GetOrCreateChildOfType<gtirb::Instruction>(this);
}
