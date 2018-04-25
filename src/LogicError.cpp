#include <gtirb/LogicError.hpp>

using namespace gtirb;

LogicError::LogicError(const char* what) : gtirb::Exception(what)
{
}

LogicError::LogicError(const std::string& what) : gtirb::Exception(what)
{
}

LogicError::LogicError(const std::string& what, std::string file, int line)
    : gtirb::Exception{what, file, line}
{
}
