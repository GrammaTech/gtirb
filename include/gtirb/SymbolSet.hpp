#ifndef GTIRB_SYMBOLSET_H
#define GTIRB_SYMBOLSET_H

#include <gtirb/Addr.hpp>
#include <map>

namespace gtirb {
class Symbol;

/// \brief DOCFIXME
using SymbolSet = std::multimap<Addr, Symbol *>;
} // namespace gtirb

#endif // GTIRB_SYMBOLSET_H
