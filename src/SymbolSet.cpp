#include <algorithm>
#include <gtirb/Module.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolSet.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::SymbolSet);

std::vector<Symbol*> SymbolSet::getSymbols() const
{
    std::vector<Symbol*> result;
    std::transform(this->contents.begin(), this->contents.end(), std::back_inserter(result),
                   [](const auto& x) { return x.get(); });
    return result;
}

std::vector<Symbol*> SymbolSet::getSymbols(gtirb::EA x) const
{
    std::vector<Symbol*> results;
    for(const auto& s : this->contents)
    {
        if(s->getEA() == x)
        {
            results.push_back(s.get());
        }
    }
    return results;
}

Symbol* SymbolSet::addSymbol(std::unique_ptr<Symbol>&& s)
{
    auto non_owning = s.get();
    this->contents.push_back(std::shared_ptr<Symbol>(std::move(s)));
    return non_owning;
}
