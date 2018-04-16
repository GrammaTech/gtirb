#include <gtirb/ModuleSectionBase.hpp>
#include <gtirb/Module.hpp>

using namespace gtirb;

ModuleSectionBase::ModuleSectionBase() : Node()
{
    this->addParentValidator([](const Node* const x) {
        // We can only be a child to a gtirb::Module.
        const auto parent = dynamic_cast<const gtirb::Module* const>(x);
        return parent != nullptr;
    });
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
