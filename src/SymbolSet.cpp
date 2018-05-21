#include <gtirb/Module.hpp>
#include <gtirb/NodeValidators.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolSet.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::SymbolSet);

SymbolSet::SymbolSet() : Node()
{
    this->addParentValidator(gtirb::NodeValidatorHasParentOfType<gtirb::Module>);
    this->addParentValidator(NodeValidatorHasNoSiblingsOfType<gtirb::SymbolSet>);
}

std::vector<Symbol*> SymbolSet::getSymbols() const
{
    return GetChildrenOfType<Symbol>(this, true);
}

std::vector<Symbol*> SymbolSet::getSymbols(gtirb::EA x) const
{
    // This grabs all symbols every time, which could be slow (measure first).
    auto symbols = getSymbols();

    symbols.erase(std::remove_if(std::begin(symbols), std::end(symbols),
                                 [x](Symbol* s) { return s->getEA() != x; }),
                  symbols.end());
    return symbols;
}

Symbol* SymbolSet::addSymbol(std::unique_ptr<Symbol>&& s)
{
    auto non_owning = s.get();
    this->push_back(std::move(s));
    return non_owning;
}
