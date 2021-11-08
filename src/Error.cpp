#include "gtirb/Error.hpp"
#include <iostream>
#include <type_traits>

namespace gtirb {

void ErrorInfoBase::anchor() {}
char ErrorList::ID = 0;
void ECError::anchor() {}
char ECError::ID = 0;
char StringError::ID = 0;
char FileError::ID = 0;

enum class ErrorErrorCode : int {
  MultipleErrors = 1,
  FileError,
  InconvertibleError
};

// FIXME: This class is only here to support the transition to llvm::Error. It
// will be removed once this transition is complete. Clients should prefer to
// deal with the Error value directly, rather than converting to error_code.
class ErrorErrorCategory : public std::error_category {
public:
  const char* name() const noexcept override { return "Error"; }

  std::string message(int condition) const override {
    switch (static_cast<ErrorErrorCode>(condition)) {
    case ErrorErrorCode::MultipleErrors:
      return "Multiple errors";
    case ErrorErrorCode::InconvertibleError:
      return "Inconvertible error value. An error has occurred that could "
             "not be converted to a known std::error_code. Please file a "
             "bug.";
    case ErrorErrorCode::FileError:
      return "A file error occurred.";
    }
    assert(!"Unhandled error code");
    return "";
  }
};

static ErrorErrorCategory ErrorErrorCat;

std::error_code ErrorList::convertToErrorCode() const {
  return std::error_code(static_cast<int>(ErrorErrorCode::MultipleErrors),
                         ErrorErrorCat);
}

StringError::StringError(std::error_code ErrCode, const char* S)
    : Msg(S), EC(ErrCode){};

StringError::StringError(const char* S, std::error_code ErrCode)
    : Msg(S), EC(ErrCode), PrintMsgOnly(true) {}

StringError::StringError(std::string&& S, std::error_code ErrCode)
    : Msg(S), EC(ErrCode), PrintMsgOnly(true) {}

std::error_code StringError::convertToErrorCode() const { return EC; }

void StringError::log(std::ostream& OS) const {
  if (PrintMsgOnly) {
    OS << Msg;
  } else {
    OS << EC.message();
    if (!Msg.empty())
      OS << (" " + Msg);
  }
}

void logAllUnhandledErrors(Error E, std::ostream& OS, std::string ErrorBanner) {
  if (!E)
    return;
  OS << ErrorBanner;
  handleAllErrors(std::move(E), [&](const ErrorInfoBase& EI) {
    EI.log(OS);
    OS << "\n";
  });
}

std::error_code inconvertibleErrorCode() {
  return std::error_code(static_cast<int>(ErrorErrorCode::InconvertibleError),
                         ErrorErrorCat);
}

std::error_code FileError::convertToErrorCode() const {
  return std::error_code(static_cast<int>(ErrorErrorCode::FileError),
                         ErrorErrorCat);
}

Error errorCodeToError(std::error_code EC) {
  if (!EC)
    return Error::success();
  return Error(std::make_unique<ECError>(ECError(EC)));
}

[[noreturn]] void report_fatal_error(const char* message) {
  std::cerr << message << "; aborting\n";
  abort();
}

std::error_code errorToErrorCode(Error Err) {
  std::error_code EC;
  handleAllErrors(std::move(Err), [&](const ErrorInfoBase& EI) {
    EC = EI.convertToErrorCode();
  });
  return EC;
}

void Error::reportUncheckedError() const {
  std::cerr << "Program containes an unhandled Error:\n";
  if (getPtr()) {
    getPtr()->log(std::cerr);
    std::cerr << "\n";
  } else
    std::cerr << "Error value was Success. (Note: Success values must still be "
                 "checked prior to being destroyed).\n";
}

} // namespace gtirb
