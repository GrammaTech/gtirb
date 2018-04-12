#include <gtirb/NodeStructureError.hpp>

using namespace gtirb;

NodeStructureError::NodeStructureError(std::string file, int line) : gtirb::NodeError{file, line}
{
}

const char* NodeStructureError::what() const noexcept
{
    return "GT-IRB Node Structure Error.";
}
