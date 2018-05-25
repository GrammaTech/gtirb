#include <gtirb/Module.hpp>
#include <gtirb/Procedure.hpp>
#include <gtirb/ProcedureSet.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::ProcedureSet);

ProcedureSet::ProcedureSet() = default;

ProcedureSet::~ProcedureSet() = default;

Procedure* ProcedureSet::getProcedure(gtirb::EA x)
{
    const auto found = this->contents.find(x);
    if(found != std::end(this->contents))
    {
        return &found->second;
    }

    return nullptr;
}

const Procedure* ProcedureSet::getProcedure(gtirb::EA x) const
{
    auto result = getProcedure(x);
    return result;
}

Procedure& ProcedureSet::createProcedure(gtirb::EA x)
{
    Expects(this->getProcedure(x) == nullptr);

    Procedure newProcedure;
    newProcedure.setEA(x);
    this->contents.insert({x, std::move(newProcedure)});

    return *this->getProcedure(x);
}

template <class Archive>
void ProcedureSet::serialize(Archive& ar, const unsigned int /*version*/)
{
    ar& boost::serialization::base_object<Node>(*this);
    ar & this->contents;
}
