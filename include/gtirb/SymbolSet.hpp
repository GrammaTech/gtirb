#pragma once

#include <gtirb/EA.hpp>

namespace gtirb
{
    class Symbol;

    using SymbolSet = std::vector<Symbol>;
    std::vector<const Symbol*> findSymbols(const SymbolSet& symbols, gtirb::EA x);
}
