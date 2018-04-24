#include <gtirb/ModuleSummary.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/NodeValidators.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::ModuleSummary);

ModuleSummary::ModuleSummary() : ModuleSectionBase()
{
    this->addParentValidator(gtirb::NodeValidatorHasParentOfType<gtirb::Module>());
    this->addParentValidator(gtirb::NodeValidatorHasNoSiblingsOfType<gtirb::ModuleSummary>());
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
