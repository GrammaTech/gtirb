#pragma once

#include <gtirb/Exception.hpp>

namespace gtirb {
///
/// \class RuntimeError
///
/// Defines a type of object to be thrown as exception. It reports errors that are due to events
/// beyond the scope of the program and can not be easily predicted.
///
class GTIRB_EXPORT_API RuntimeError : public gtirb::Exception {
public:
  ///
  /// Implements a constructor from the base type.
  ///
  /// \param  what    A helpful, descriptive message to pass along with this exception.
  ///
  RuntimeError(const char* what = "GT-IRB Runtime Error.");

  ///
  /// Implements a constructor from the base type.
  ///
  /// \param  what    A helpful, descriptive message to pass along with this exception.
  ///
  RuntimeError(const std::string& what);

  ///
  /// A constructor to track the file name and line number where the exception was generated.
  ///
  /// \param  what    A helpful, descriptive message to pass along with this exception.
  /// \param  file    The file name that generated this exception.
  /// \param  line    The line number within the file that generated this exception.
  ///
  RuntimeError(const std::string& what, std::string file, int line);

  ///
  /// A defaulted trivial destructor.
  ///
  ~RuntimeError() override = default;
};
}
