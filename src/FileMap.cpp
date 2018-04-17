#include <gtirb/FileMap.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/NodeValidators.hpp>

using namespace gtirb;

FileMap::FileMap() : Node()
{
	this->addParentValidator(gtirb::NodeValidatorHasParentOfType<gtirb::Module>());
}
