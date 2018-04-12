#include <gtirb/LogicError.hpp>

using namespace gtirb;

LogicError::LogicError(std::string file, int line) : gtirb::Exception{file, line}
{
}

const char* LogicError::what() const noexcept
{
    return "GT-IRB Logic Error.";
}
