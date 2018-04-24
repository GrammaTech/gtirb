#include <gtirb/ModuleAux.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/NodeValidators.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::ModuleAux);

ModuleAux::ModuleAux() : ModuleSectionBase()
{
    this->addParentValidator(gtirb::NodeValidatorHasParentOfType<gtirb::Module>());
    this->addParentValidator(gtirb::NodeValidatorHasNoSiblingsOfType<gtirb::ModuleAux>());
}
