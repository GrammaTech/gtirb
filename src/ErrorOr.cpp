#include <ErrorOr.hpp>

namespace gtirb {

ErrorInfo createStringError(std::error_code EC, const std::string& Msg) {
  return ErrorInfo{EC, Msg};
}

ErrorInfo& joinErrors(ErrorInfo& Error, const std::string& Msg,
                      const std::string& sep) {
  Error.Msg += sep;
  Error.Msg += Msg;
  return Error;
}

ErrorInfo& joinErrors(ErrorInfo& Error, const ErrorInfo& Info,
                      const std::string& sep) {
  std::stringstream ss;
  ss << Info;
  return joinErrors(Error, ss.str(), sep);
}

} // namespace gtirb
