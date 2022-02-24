#include <ErrorOr.hpp>
#include <sstream>

namespace gtirb {

std::string ErrorInfo::message() const {
  std::stringstream Stream;
  Stream << *this;
  return Stream.str();
}

} // namespace gtirb
