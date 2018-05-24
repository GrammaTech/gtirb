#include <gsl/gsl>
#include <gtirb/CFGNode.hpp>
#include <gtirb/CFGNodeInfo.hpp>
#include <gtirb/RuntimeError.hpp>
#include <gtirb/Symbol.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::CFGNodeInfo);

void CFGNodeInfo::setProcedureNameSymbol(gtirb::Symbol* x)
{
    auto sharedNode = std::dynamic_pointer_cast<Symbol>(x->shared_from_this());
    Expects(sharedNode != nullptr);
    this->procedureNameSymbol = sharedNode;
}

gtirb::Symbol* CFGNodeInfo::getProcedureNameSymbol() const
{
    return this->procedureNameSymbol.lock().get();
}
