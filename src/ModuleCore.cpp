#include <gtirb/ModuleCore.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/NodeValidators.hpp>

using namespace gtirb;

ModuleCore::ModuleCore() : ModuleSectionBase()
{
    this->addParentValidator(gtirb::NodeValidatorHasParentOfType<gtirb::Module>());
    this->addParentValidator(gtirb::NodeValidatorHasNoSiblingsOfType<gtirb::ModuleCore>());
}
