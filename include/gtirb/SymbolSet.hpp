#ifndef GTIRB_SYMBOLSET_H
#define GTIRB_SYMBOLSET_H

#include <gtirb/Addr.hpp>
#include <map>

namespace gtirb {
class Symbol;


/// \brief DOCFIXME
using SymbolSet = std::multimap<Addr, Symbol>;


/// DOCFIXME[check all]
///
/// \brief Add a Symbol to a SymbolSet.
///
/// \param Symbols  The SymbolSet to be modified.
///
/// \param Sym      The Symbol to add.
///  
GTIRB_EXPORT_API void addSymbol(SymbolSet& Symbols, Symbol&& Sym);


/// DOCFIXME[check all]
///
/// \brief Find all symbols in a set that have the specified effective address.
///
/// \param Symbols  The SymbolSet to be searched
///
/// \param X        The effective address of interest
///
/// \return A std::vector<const Symbol*> containing all symbols (\ref
/// Symbol) in Symbols whose effective address is X.
/// 
GTIRB_EXPORT_API std::vector<Symbol*> findSymbols(SymbolSet& Symbols, Addr X);
/// DOCFIXME[check all]
///
/// \brief Find all symbols in a set that have the specified effective address. DOCFIXME[difference to previous]
///
/// \param Symbols  The SymbolSet to be searched
///
/// \param X        The effective address of interest
///
/// \return A std::vector<const Symbol*> containing all symbols (\ref
/// Symbol) in Symbols whose effective address is X.
///
GTIRB_EXPORT_API std::vector<const Symbol*>
findSymbols(const SymbolSet& Symbols, Addr X);
} // namespace gtirb

#endif // GTIRB_SYMBOLSET_H
