#include <algorithm>
#include <gtirb/Module.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolSet.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::SymbolSet);

const std::vector<Symbol>& SymbolSet::getSymbols() const
{
    return this->contents;
}

std::vector<const Symbol*> SymbolSet::getSymbols(gtirb::EA x) const
{
    std::vector<const Symbol*> results;
    for(const auto& s : this->contents)
    {
        if(s.getEA() == x)
        {
            results.push_back(&s);
        }
    }
    return results;
}

Symbol& SymbolSet::addSymbol(Symbol&& s)
{
    this->contents.push_back(std::move(s));
    return this->contents.back();
}
