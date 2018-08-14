#pragma once

#include <gtirb/EA.hpp>
#include <map>

namespace gtirb {
class Symbol;

using SymbolSet = std::multimap<EA, Symbol>;

GTIRB_EXPORT_API void addSymbol(SymbolSet& Symbols, Symbol&& Sym);
GTIRB_EXPORT_API std::vector<Symbol*> findSymbols(SymbolSet& Symbols, EA X);
GTIRB_EXPORT_API std::vector<const Symbol*>
findSymbols(const SymbolSet& Symbols, EA X);
} // namespace gtirb
