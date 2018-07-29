#pragma once

#include <boost/variant.hpp>
#include <gtirb/EA.hpp>

namespace gtirb {
struct SymStackConst;
struct SymAddrConst;
struct SymAddrAddr;

using SymbolicExpression = boost::variant<SymStackConst, SymAddrConst, SymAddrAddr>;
using SymbolicExpressionSet = std::map<EA, SymbolicExpression>;
}
