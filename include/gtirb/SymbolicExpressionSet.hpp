#ifndef GTIRB_SYMBOLICEXPRESSIONSET_H
#define GTIRB_SYMBOLICEXPRESSIONSET_H

#include <gtirb/Addr.hpp>
#include <map>
#include <variant>

namespace gtirb {
struct SymStackConst;
struct SymAddrConst;
struct SymAddrAddr;

using SymbolicExpression =
    std::variant<SymStackConst, SymAddrConst, SymAddrAddr>;
using SymbolicExpressionSet = std::map<Addr, SymbolicExpression>;
} // namespace gtirb

#endif // GTIRB_SYMBOLICEXPRESSIONSET_H
