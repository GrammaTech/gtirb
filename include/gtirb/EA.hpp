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
/// An EA cannot store a relative address as it cannot contain a negative
/// number.
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
  constexpr explicit EA(uint64_t X) : Address(X) {}
  constexpr explicit EA(int64_t X) : Address(X) {}
  constexpr explicit EA(uint32_t X) : Address(X) {}
  constexpr explicit EA(int32_t X) : Address(X) {}
  constexpr explicit EA(uint16_t X) : Address(X) {}
  constexpr explicit EA(int16_t X) : Address(X) {}
  constexpr explicit EA(uint8_t X) : Address(X) {}
  constexpr explicit EA(int8_t X) : Address(X) {}

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
  void set(uint64_t X);

  ///
  /// Get the value of the EA as a 'uint64_t'.
  ///
  uint64_t get() const;

  ///
  /// Assignment operator overload
  ///
  EA& operator=(EA X);

  ///
  /// Allow testing for equality with a 'uint64_t'.
  ///
  bool operator==(uint64_t X) const;

  bool operator==(EA X) const;
  bool operator!=(EA X) const;
  bool operator>(EA X) const;
  bool operator<(EA X) const;

  EA operator+(EA X) const;
  EA operator+(uint64_t X) const;
  EA& operator+=(EA X);
  EA& operator+=(uint64_t X);
  EA operator-(EA X) const;
  EA operator-(uint64_t X) const;
  EA& operator-=(const EA X);
  EA& operator-=(const uint64_t X);

  ///
  /// Provide for static casting to a 'std::string'.
  ///
  operator std::string() const;

private:
  ///
  /// Prevent automatic type conversions.
  /// This template is not implemented as to provide compiler protection against
  /// automatic type conversions.
  ///
  template <typename T> void set(T);

  ///
  /// Prevent automatic type conversions.
  /// This template is not implemented as to provide compiler protection against
  /// automatic type conversions.
  ///
  template <typename T> EA operator+(const T) const;

  ///
  /// Prevent automatic type conversions.
  /// This template is not implemented as to provide compiler protection against
  /// automatic type conversions.
  ///
  template <typename T> EA operator+=(const T);

  ///
  /// Prevent automatic type conversions.
  /// This template is not implemented as to provide compiler protection against
  /// automatic type conversions.
  ///
  template <typename T> EA operator-(const T) const;

  ///
  /// Prevent automatic type conversions.
  /// This template is not implemented as to provide compiler protection against
  /// automatic type conversions.
  ///
  template <typename T> EA operator-=(const T);

  ///
  /// Internal storage for the effective address (EA).
  /// It is initialized with the constant BadAddress.
  ///
  uint64_t Address{BadAddress};
};

///
/// Exclusive limit of object's address range.
///
/// Object can be any type which specifies a range of addresses via
/// getAddress() and getSize() methods (e.g. DataObject).
///
template <typename T> EA addressLimit(const T& Object) {
  return Object.getAddress() + Object.getSize();
}

///
/// Does object contain a given EA?
///
/// Object can be any type which specifies a range of addresses via
/// getAddress() and getSize() methods (e.g. DataObject).
///
template <typename T> bool containsEA(const T& Object, EA Ea) {
  if (Object.getAddress() == EA()) {
    return false;
  } else {
    return Object.getAddress() <= Ea && addressLimit(Object) > Ea;
  }
}
} // namespace gtirb

///
/// Provide for testing equality between a gtirb::EA and a uint64_t.
///
inline bool operator==(const uint64_t Rhs, const gtirb::EA Lhs) {
  return gtirb::EA(Rhs) == Lhs;
}

///
/// Print EA to a stream
///
GTIRB_EXPORT_API std::ostream& operator<<(std::ostream& os,
                                          const gtirb::EA& ea);
