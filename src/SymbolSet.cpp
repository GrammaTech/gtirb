#include <gtirb/EA.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolSet.hpp>

namespace gtirb
{
    std::vector<const Symbol*> findSymbols(const SymbolSet& symbols, gtirb::EA x)
    {
        std::vector<const Symbol*> results;
        for(const auto& s : symbols)
        {
            if(s.getEA() == x)
            {
                results.push_back(&s);
            }
        }
        return results;
    }
}
