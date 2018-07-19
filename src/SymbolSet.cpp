#include <gtirb/EA.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolSet.hpp>

namespace gtirb
{
    void addSymbol(SymbolSet& symbols, Symbol&& sym)
    {
        symbols.emplace(sym.getEA(), std::move(sym));
    }

    std::vector<const Symbol*> findSymbols(const SymbolSet& symbols, gtirb::EA x)
    {
        auto found = symbols.equal_range(x);
        std::vector<const Symbol*> result;
        std::for_each(found.first, found.second,
                      [&result](const auto& node) { result.push_back(&node.second); });
        return result;
    }
}
