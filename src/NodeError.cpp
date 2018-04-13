#include <gtirb/NodeError.hpp>

using namespace gtirb;

NodeError::NodeError(const char* what) : gtirb::Exception(what)
{
}

NodeError::NodeError(const std::string& what) : gtirb::Exception(what)
{
}

NodeError::NodeError(const std::string& what, std::string file, int line) : gtirb::Exception{what, file, line}
{
}

void NodeError::setNodeType(std::string x)
{
    this->nodeType = std::move(x);
}

std::string NodeError::getNodeType() const
{
    return this->nodeType;
}
