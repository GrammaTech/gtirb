#include <gtirb/Module.hpp>
#include <gtirb/Procedure.hpp>
#include <gtirb/ProcedureSet.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::ProcedureSet);

Procedure* ProcedureSet::getProcedure(gtirb::EA x) const
{
    const auto found = this->contents.find(x);
    if(found != std::end(this->contents))
    {
        return found->second.get();
    }

    return nullptr;
}

Procedure* ProcedureSet::createProcedure(gtirb::EA x)
{
    Expects(this->getProcedure(x) == nullptr);

    auto newProcedure = std::make_shared<Procedure>();
    newProcedure->setEA(x);
    auto procedure = newProcedure.get();
    this->contents.insert({x, std::move(newProcedure)});

    return procedure;
}
