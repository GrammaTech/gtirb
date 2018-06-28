#pragma once

#include <boost/variant.hpp>
#include <gtirb/EA.hpp>

namespace gtirb
{
    struct SymStackConst;
    struct SymAddrConst;
    struct SymAddrAddr;

    using SymbolicOperand = boost::variant<SymStackConst, SymAddrConst, SymAddrAddr>;
    using SymbolicOperandSet = std::map<EA, SymbolicOperand>;
}
