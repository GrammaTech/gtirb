#include <gtirb/LoadedFileMap.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/NodeValidators.hpp>

using namespace gtirb;

LoadedFileMap::LoadedFileMap() : Node()
{
	this->addParentValidator(gtirb::NodeValidatorHasParentOfType<gtirb::Module>());
}
