#include "LogicError.hpp"

using namespace gtirb;

LogicError::LogicError(const char* what_) : gtirb::Exception(what_) {}

LogicError::LogicError(const std::string& what_) : gtirb::Exception(what_) {}

LogicError::LogicError(const std::string& what_, std::string file_, int line_)
    : gtirb::Exception{what_, file_, line_} {}
