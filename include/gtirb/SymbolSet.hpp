#pragma once

#include <gtirb/Addr.hpp>
#include <map>

namespace gtirb {
class Symbol;

using SymbolSet = std::multimap<Addr, Symbol>;

GTIRB_EXPORT_API void addSymbol(SymbolSet& Symbols, Symbol&& Sym);
GTIRB_EXPORT_API std::vector<Symbol*> findSymbols(SymbolSet& Symbols, Addr X);
GTIRB_EXPORT_API std::vector<const Symbol*>
findSymbols(const SymbolSet& Symbols, Addr X);
} // namespace gtirb
