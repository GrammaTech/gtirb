#include <gtirb/RuntimeError.hpp>

using namespace gtirb;

RuntimeError::RuntimeError(std::string file, int line) : gtirb::Exception{file, line}
{
}

const char* RuntimeError::what() const noexcept
{
	return "GT-IRB Runtime Error.";
}
