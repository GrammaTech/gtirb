#include <gtirb/Module.hpp>
#include <gtirb/NodeValidators.hpp>
#include <gtirb/Procedure.hpp>
#include <gtirb/ProcedureSet.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::ProcedureSet);

ProcedureSet::ProcedureSet() : Node()
{
    this->addParentValidator(gtirb::NodeValidatorHasParentOfType<gtirb::Module>());
    this->addParentValidator(NodeValidatorHasNoSiblingsOfType<gtirb::ProcedureSet>());
}

Procedure* ProcedureSet::getProcedure(gtirb::EA x) const
{
    const auto procedures = GetChildrenOfType<Procedure>(this);
    const auto found = std::find_if(std::begin(procedures), std::end(procedures),
                                    [x](Procedure* s) { return s->getEA() == x; });

    if(found != std::end(procedures))
    {
        return *found;
    }

    return nullptr;
}

Procedure* ProcedureSet::getOrCreateProcedure(gtirb::EA x)
{
    auto procedure = this->getProcedure(x);

    if(procedure == nullptr)
    {
        auto newProcedure = std::make_unique<Procedure>();
        newProcedure->setEA(x);
        procedure = newProcedure.get();
        this->push_back(std::move(newProcedure));
    }

    return procedure;
}
