#include <gsl/gsl>
#include <gtirb/CFGNode.hpp>
#include <gtirb/CFGNodeInfo.hpp>
#include <gtirb/RuntimeError.hpp>
#include <gtirb/Symbol.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::CFGNodeInfo);

void CFGNodeInfo::setProcedureNameSymbol(gtirb::Symbol* x)
{
    this->procedureNameSymbol = x;
}

gtirb::Symbol* CFGNodeInfo::getProcedureNameSymbol() const
{
    return this->procedureNameSymbol;
}
