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
/// The initial value for an Addr.
///
constexpr uint64_t BadAddress{std::numeric_limits<uint64_t>::max()};

///
/// \class Addr
///
/// A special class to store an Effective Address. This is initialized to
/// `BadAddress`. It is compatible with a uint64_t for 64-bit address storage.
/// An Addr cannot store a relative address as it cannot contain a negative
/// number.
///
class GTIRB_EXPORT_API Addr {
public:
  ///
  /// Default constructor.
  /// The default implementation provided by the compiler is used.
  ///
  constexpr Addr() = default;

  ///
  /// Allow explicit conversion from all integer types to Addr.
  ///
  constexpr explicit Addr(uint64_t X) : Address(X) {}
  constexpr explicit Addr(int64_t X) : Address(X) {}
  constexpr explicit Addr(uint32_t X) : Address(X) {}
  constexpr explicit Addr(int32_t X) : Address(X) {}
  constexpr explicit Addr(uint16_t X) : Address(X) {}
  constexpr explicit Addr(int16_t X) : Address(X) {}
  constexpr explicit Addr(uint8_t X) : Address(X) {}
  constexpr explicit Addr(int8_t X) : Address(X) {}

  ///
  /// Allow implicit conversion from Addr to uint64_t.
  ///
  operator uint64_t() const;

  ///
  /// Allow explicit conversion from Addr to all other integer types.
  ///
  explicit operator int64_t() const;
  explicit operator uint32_t() const;
  explicit operator int32_t() const;
  explicit operator uint16_t() const;
  explicit operator int16_t() const;
  explicit operator uint8_t() const;
  explicit operator int8_t() const;

  ///
  /// Explicitly set the value of the Addr from a 'uint64_t'.
  ///
  void set(uint64_t X);

  ///
  /// Get the value of the Addr as a 'uint64_t'.
  ///
  uint64_t get() const;

  ///
  /// Assignment operator overload
  ///
  Addr& operator=(Addr X);

  ///
  /// Allow testing for equality with a 'uint64_t'.
  ///
  bool operator==(uint64_t X) const;

  bool operator==(Addr X) const;
  bool operator!=(Addr X) const;
  bool operator>(Addr X) const;
  bool operator<(Addr X) const;

  Addr operator+(Addr X) const;
  Addr operator+(uint64_t X) const;
  Addr& operator+=(Addr X);
  Addr& operator+=(uint64_t X);
  Addr operator-(Addr X) const;
  Addr operator-(uint64_t X) const;
  Addr& operator-=(const Addr X);
  Addr& operator-=(const uint64_t X);

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
  template <typename T> Addr operator+(const T) const;

  ///
  /// Prevent automatic type conversions.
  /// This template is not implemented as to provide compiler protection against
  /// automatic type conversions.
  ///
  template <typename T> Addr operator+=(const T);

  ///
  /// Prevent automatic type conversions.
  /// This template is not implemented as to provide compiler protection against
  /// automatic type conversions.
  ///
  template <typename T> Addr operator-(const T) const;

  ///
  /// Prevent automatic type conversions.
  /// This template is not implemented as to provide compiler protection against
  /// automatic type conversions.
  ///
  template <typename T> Addr operator-=(const T);

  ///
  /// Internal storage for the effective address (Addr).
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
template <typename T> Addr addressLimit(const T& Object) {
  return Object.getAddress() + Object.getSize();
}

///
/// Does object contain a given Addr?
///
/// Object can be any type which specifies a range of addresses via
/// getAddress() and getSize() methods (e.g. DataObject).
///
template <typename T> bool containsAddr(const T& Object, Addr Ea) {
  if (Object.getAddress() == Addr()) {
    return false;
  } else {
    return Object.getAddress() <= Ea && addressLimit(Object) > Ea;
  }
}
} // namespace gtirb

///
/// Provide for testing equality between a gtirb::Addr and a uint64_t.
///
inline bool operator==(const uint64_t Rhs, const gtirb::Addr Lhs) {
  return gtirb::Addr(Rhs) == Lhs;
}

///
/// Print Addr to a stream
///
GTIRB_EXPORT_API std::ostream& operator<<(std::ostream& os,
                                          const gtirb::Addr& ea);
