#pragma once

#include <gtirb/Export.hpp>
#include <cstdint>
#include <iostream>
#include <limits>
#include <string>

namespace gtirb {
///
/// \var BadAddress
///
/// The initial value for an EA.
///
constexpr uint64_t BadAddress{std::numeric_limits<uint64_t>::max()};

///
/// \class EA
///
/// A special class to store an Effective Address. This is initialized to
/// `BadAddress`. It is compatible with a uint64_t for 64-bit address storage.
/// An EA cannot store a relative address as it cannot contain a negative number.
///
class GTIRB_EXPORT_API EA {
public:
  ///
  /// Default constructor.
  /// The default implementation provided by the compiler is used.
  ///
  constexpr EA() = default;

  ///
  /// Allow explicit conversion from all integer types to EA.
  ///
  constexpr explicit EA(uint64_t x) : ea(x) {}
  constexpr explicit EA(int64_t x) : ea(x) {}
  constexpr explicit EA(uint32_t x) : ea(x) {}
  constexpr explicit EA(int32_t x) : ea(x) {}
  constexpr explicit EA(uint16_t x) : ea(x) {}
  constexpr explicit EA(int16_t x) : ea(x) {}
  constexpr explicit EA(uint8_t x) : ea(x) {}
  constexpr explicit EA(int8_t x) : ea(x) {}

  ///
  /// Allow implicit conversion from EA to uint64_t.
  ///
  operator uint64_t() const;

  ///
  /// Allow explicit conversion from EA to all other integer types.
  ///
  explicit operator int64_t() const;
  explicit operator uint32_t() const;
  explicit operator int32_t() const;
  explicit operator uint16_t() const;
  explicit operator int16_t() const;
  explicit operator uint8_t() const;
  explicit operator int8_t() const;

  ///
  /// Explicitly set the value of the EA from a 'uint64_t'.
  ///
  void set(uint64_t x);

  ///
  /// Get the value of the EA as a 'uint64_t'.
  ///
  uint64_t get() const;

  ///
  /// Assignment operator overload
  ///
  EA& operator=(EA x);

  ///
  /// Allow testing for equality with a 'uint64_t'.
  ///
  bool operator==(uint64_t x) const;

  bool operator==(EA x) const;
  bool operator!=(EA x) const;
  bool operator>(EA x) const;
  bool operator<(EA x) const;

  EA operator+(EA x) const;
  EA operator+(uint64_t x) const;
  EA& operator+=(EA x);
  EA& operator+=(uint64_t x);
  EA operator-(EA x) const;
  EA operator-(uint64_t x) const;
  EA& operator-=(const EA x);
  EA& operator-=(const uint64_t x);

  ///
  /// Provide for static casting to a 'std::string'.
  ///
  operator std::string() const;

private:
  ///
  /// Prevent automatic type conversions.
  /// This template is not implemented as to provide compiler protection against automatic
  /// type conversions.
  ///
  template <typename T> void set(T);

  ///
  /// Prevent automatic type conversions.
  /// This template is not implemented as to provide compiler protection against automatic
  /// type conversions.
  ///
  template <typename T> EA operator+(const T) const;

  ///
  /// Prevent automatic type conversions.
  /// This template is not implemented as to provide compiler protection against automatic
  /// type conversions.
  ///
  template <typename T> EA operator+=(const T);

  ///
  /// Prevent automatic type conversions.
  /// This template is not implemented as to provide compiler protection against automatic
  /// type conversions.
  ///
  template <typename T> EA operator-(const T) const;

  ///
  /// Prevent automatic type conversions.
  /// This template is not implemented as to provide compiler protection against automatic
  /// type conversions.
  ///
  template <typename T> EA operator-=(const T);

  ///
  /// Internal storage for the effective address (EA).
  /// It is initialized with the constant BadAddress.
  ///
  uint64_t ea{BadAddress};
};
} // namespace gtirb

///
/// Provide for testing equality between a gtirb::EA and a uint64_t.
///
inline bool operator==(const uint64_t rhs, const gtirb::EA lhs) { return gtirb::EA(rhs) == lhs; }

///
/// Print EA to a stream
///
std::ostream& operator<<(std::ostream& os, const gtirb::EA& ea);
