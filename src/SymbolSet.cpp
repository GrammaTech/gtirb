#include <gtirb/SymbolSet.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/NodeValidators.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::SymbolSet);

SymbolSet::SymbolSet() : Node()
{
	this->addParentValidator(gtirb::NodeValidatorHasParentOfType<gtirb::Module>());
	this->addParentValidator(NodeValidatorHasNoSiblingsOfType<gtirb::SymbolSet>());
}

Symbol* SymbolSet::getSymbol(gtirb::EA x) const
{
	const auto symbols = GetChildrenOfType<Symbol>(this);
	const auto found = std::find_if(std::begin(symbols), std::end(symbols), [x](Symbol* s){return s->getEA() == x;});
	
	if(found != std::end(symbols))
	{
		return *found;
	}

	return nullptr;
}

Symbol* SymbolSet::getOrCreateSymbol(gtirb::EA x)
{
	auto symbol = this->getSymbol(x);

	if(symbol == nullptr)
	{
		auto newSymbol = std::make_unique<Symbol>(x);
		symbol = newSymbol.get();
		this->push_back(std::move(newSymbol));
	}

	return symbol;
}
