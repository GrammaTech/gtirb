#include <gtirb/Instruction.hpp>
#include <gtirb/Procedure.hpp>
#include <gtirb/ProcedureSet.hpp>
#include <gtirb/RuntimeError.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::Procedure);

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

Instruction* Procedure::createInstruction()
{
    this->instructions.emplace_back();
    return &this->instructions.back();
}
