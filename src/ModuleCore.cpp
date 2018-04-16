#include <gtirb/ModuleCore.hpp>
#include <gtirb/Module.hpp>

using namespace gtirb;

ModuleCore::ModuleCore() : ModuleSectionBase()
{
    this->addParentValidator([](const Node* const x) {
        // We can only be a child to a gtirb::Module.
        const auto parent = dynamic_cast<const gtirb::Module* const>(x);
        if(parent != nullptr)
        {
        	// We should have no siblings.
        	const auto siblings = GetChildrenOfType<gtirb::ModuleCore>(parent);
        	return siblings.empty();
        }

        return false;
    });
}
