#pragma once

#include <gtirb/EA.hpp>
#include <boost/variant.hpp>
#include <map>

namespace gtirb {
struct SymStackConst;
struct SymAddrConst;
struct SymAddrAddr;

using SymbolicExpression =
    boost::variant<SymStackConst, SymAddrConst, SymAddrAddr>;
using SymbolicExpressionSet = std::map<EA, SymbolicExpression>;
} // namespace gtirb
