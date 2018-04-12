#include <gtirb/NodeError.hpp>

using namespace gtirb;

NodeError::NodeError(std::string file, int line) : gtirb::Exception{file, line}
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

const char* NodeError::what() const noexcept
{
    return "GT-IRB Node Error.";
}
