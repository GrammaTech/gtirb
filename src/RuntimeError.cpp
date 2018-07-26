#include <gtirb/RuntimeError.hpp>

using namespace gtirb;

RuntimeError::RuntimeError(const char* what_) : gtirb::Exception(what_) {}

RuntimeError::RuntimeError(const std::string& what_) : gtirb::Exception(what_) {}

RuntimeError::RuntimeError(const std::string& what_, std::string file_, int line_)
    : gtirb::Exception{what_, file_, line_} {}
