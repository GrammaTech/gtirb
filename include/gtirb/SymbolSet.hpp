#pragma once

#include <gtirb/EA.hpp>
#include <map>

namespace gtirb {
class Symbol;

using SymbolSet = std::multimap<EA, Symbol>;

void addSymbol(SymbolSet& symbols, Symbol&& sym);
std::vector<Symbol*> findSymbols(SymbolSet& symbols, EA x);
std::vector<const Symbol*> findSymbols(const SymbolSet& symbols, EA x);
} // namespace gtirb
