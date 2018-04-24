#include <gtirb/CFG.hpp>
#include <gtirb/EA.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/NodeValidators.hpp>
#include <gtirb/RuntimeError.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::CFG);

CFG::CFG() : Node()
{
    this->addParentValidator(gtirb::NodeValidatorHasParentOfType<gtirb::Module>());
    this->addParentValidator(gtirb::NodeValidatorHasNoSiblingsOfType<gtirb::CFG>());
}
