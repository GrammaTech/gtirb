#include <gtirb/Instruction.hpp>
#include <gtirb/NodeValidators.hpp>
#include <gtirb/Procedure.hpp>
#include <gtirb/ProcedureSet.hpp>
#include <gtirb/RuntimeError.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::Procedure);

Procedure::Procedure() : Node()
{
    this->addParentValidator(gtirb::NodeValidatorHasParentOfType<gtirb::ProcedureSet>);
}

void Procedure::setEA(gtirb::EA x)
{
    this->ea = x;
}

gtirb::EA Procedure::getEA() const
{
    return this->ea;
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
