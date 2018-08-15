#pragma once

#include <gtirb/EA.hpp>
#include <variant>
#include <map>

namespace gtirb {
struct SymStackConst;
struct SymAddrConst;
struct SymAddrAddr;

using SymbolicExpression =
    std::variant<SymStackConst, SymAddrConst, SymAddrAddr>;
using SymbolicExpressionSet = std::map<EA, SymbolicExpression>;
} // namespace gtirb
