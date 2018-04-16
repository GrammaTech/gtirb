#include <gtirb/ModuleAux.hpp>
#include <gtirb/Module.hpp>

using namespace gtirb;

ModuleAux::ModuleAux() : ModuleSectionBase()
{
    this->addParentValidator([](const Node* const x) {
        // We can only be a child to a gtirb::Module.
        const auto parent = dynamic_cast<const gtirb::Module* const>(x);
        if(parent != nullptr)
        {
        	// We should have no siblings.
        	const auto siblings = GetChildrenOfType<gtirb::ModuleAux>(parent);
        	return siblings.empty();
        }

        return false;
    });
}
