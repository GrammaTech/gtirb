#ifndef GTIRB_SYMBOLICEXPRESSIONSET_H
#define GTIRB_SYMBOLICEXPRESSIONSET_H

#include <gtirb/Addr.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <map>
#include <variant>

namespace gtirb {
/// \brief DOCFIXME  
using SymbolicExpressionSet = std::map<Addr, SymbolicExpression>;
} // namespace gtirb

#endif // GTIRB_SYMBOLICEXPRESSIONSET_H
