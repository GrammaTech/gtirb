#pragma once

#include <gtirb/Export.hpp>
#include <stdexcept>
#include <string>
#include <utility>

namespace gtirb {
///
/// \class Exception
///
/// The base class for all GT-IRB exceptions.
/// Compatible with 'std::exception'. Provide an error message in the constructor for better
/// tracking.  This class allows for setting and retrieving the source code location that
/// generated the exception.
///
class GTIRB_EXPORT_API Exception : public std::logic_error {
public:
  ///
  /// Implements a constructor from the base type.
  ///
  /// \param  what    A helpful, descriptive message to pass along with this exception.
  ///
  Exception(const char* what = "GT-IRB Exception.");

  ///
  /// Implements a constructor from the base type.
  ///
  /// \param  what    A helpful, descriptive message to pass along with this exception.
  ///
  Exception(const std::string& what);

  ///
  /// A constructor to track the file name and line number where the exception was generated.
  ///
  /// \param  what    A helpful, descriptive message to pass along with this exception.
  /// \param  file    The file name that generated this exception.
  /// \param  line    The line number within the file that generated this exception.
  ///
  Exception(const std::string& what, std::string file, int line);

  ///
  /// A defaulted trivial destructor.
  ///
  ~Exception() override = default;

  ///
  /// Explicitly set the location within the code that generated the exception.
  ///
  /// \param  file    The file name that generated this exception.
  /// \param  line    The line number within the file that generated this exception.
  ///
  /// \code{.cpp}
  /// auto e = gtirb::Exception("Demo Exception");
  /// e.setLocation(__FILE__, __LINE__);
  /// \endcode
  ///
  void setLocation(std::string file, int line);

  ///
  /// Get the location within the source code that generated this exception.
  ///
  /// \return A pair containing the source code file name and line number within that file
  /// that generated this exception.
  ///
  std::pair<std::string, int> getLocation() const;

private:
  /// The file name which generated the exception.
  std::string file{};

  /// The line number within the file that generated the exception.
  int line{0};
};
} // namespace gtirb
