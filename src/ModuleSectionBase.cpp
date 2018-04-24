#include <gtirb/ModuleSectionBase.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/NodeValidators.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::ModuleSectionBase);

ModuleSectionBase::ModuleSectionBase() : Node()
{
	this->addParentValidator(gtirb::NodeValidatorHasParentOfType<gtirb::Module>());
}

bool ModuleSectionBase::getIsSetupComplete() const
{
    return this->isSetupComplete;
}

bool ModuleSectionBase::getIsReadOnly() const
{
    return this->isReadOnly;
}

void ModuleSectionBase::setIsSetupComplete()
{
    this->isSetupComplete = true;
}

void ModuleSectionBase::setIsReadOnly(bool x)
{
    this->isReadOnly = x;
}
