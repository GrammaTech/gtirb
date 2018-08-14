#pragma once

#include <gtirb/EA.hpp>
#include <map>

namespace gtirb {
class Symbol;

using SymbolSet = std::multimap<EA, Symbol>;

GTIRB_EXPORT_API void addSymbol(SymbolSet& symbols, Symbol&& sym);
GTIRB_EXPORT_API std::vector<Symbol*> findSymbols(SymbolSet& symbols, EA x);
GTIRB_EXPORT_API std::vector<const Symbol*>
findSymbols(const SymbolSet& symbols, EA x);
} // namespace gtirb
