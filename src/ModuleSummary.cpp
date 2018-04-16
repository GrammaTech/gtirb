#include <gtirb/ModuleSummary.hpp>
#include <gtirb/Module.hpp>

using namespace gtirb;

ModuleSummary::ModuleSummary() : ModuleSectionBase()
{
    this->addParentValidator([](const Node* const x) {
        // We can only be a child to a gtirb::Module.
        const auto parent = dynamic_cast<const gtirb::Module* const>(x);
        if(parent != nullptr)
        {
        	// We should have no siblings.
        	const auto siblings = GetChildrenOfType<gtirb::ModuleSummary>(parent);
        	return siblings.empty();
        }

        return false;
    });
}

void ModuleSummary::setName(std::string x)
{
    this->name = std::move(x);
}

std::string ModuleSummary::getName() const
{
    return this->name;
}

void ModuleSummary::setDecodeMode(uint64_t x)
{
    this->decodeMode = x;
}

uint64_t ModuleSummary::getDecodeMode() const
{
    return this->decodeMode;
}
