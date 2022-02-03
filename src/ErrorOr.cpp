#include <ErrorOr.hpp>
#include <sstream>

namespace gtirb {

ErrorInfo& joinErrors(ErrorInfo& Error, const std::string& Msg,
                      const std::string& sep) {
  Error.Msg += (sep + Msg);
  return Error;
}

ErrorInfo& joinErrors(ErrorInfo& Error, const ErrorInfo& Info,
                      const std::string& sep) {
  std::stringstream Stream;
  Stream << Info;
  return joinErrors(Error, Stream.str(), sep);
}

} // namespace gtirb
