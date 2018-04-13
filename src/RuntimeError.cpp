#include <gtirb/RuntimeError.hpp>

using namespace gtirb;

RuntimeError::RuntimeError(const char* what) : gtirb::Exception(what)
{
}

RuntimeError::RuntimeError(const std::string& what) : gtirb::Exception(what)
{
}

RuntimeError::RuntimeError(const std::string& what, std::string file, int line) : gtirb::Exception{what, file, line}
{
}
