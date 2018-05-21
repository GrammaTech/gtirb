#include <gtirb/Module.hpp>
#include <gtirb/ModuleCore.hpp>
#include <gtirb/NodeValidators.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::ModuleCore);

ModuleCore::ModuleCore() : ModuleSectionBase()
{
    this->addParentValidator(gtirb::NodeValidatorHasParentOfType<gtirb::Module>);
    this->addParentValidator(gtirb::NodeValidatorHasNoSiblingsOfType<gtirb::ModuleCore>);
}
