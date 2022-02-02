#include <ErrorOr.hpp>
#include <sstream>

namespace gtirb {

std::string ErrorInfo::asString() const {
  std::stringstream Stream;
  Stream << ErrorCode.message();
  if (Msg.length()) {
    Stream << " " << Msg;
  }
  return Stream.str();
}

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
  return joinErrors(Error, Info.asString(), sep);
}

} // namespace gtirb
