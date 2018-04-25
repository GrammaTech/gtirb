#include <gtirb/NodeStructureError.hpp>

using namespace gtirb;

NodeStructureError::NodeStructureError(const char* what) : gtirb::NodeError(what)
{
}

NodeStructureError::NodeStructureError(const std::string& what) : gtirb::NodeError(what)
{
}

NodeStructureError::NodeStructureError(const std::string& what, std::string file, int line)
    : gtirb::NodeError{what, file, line}
{
}
