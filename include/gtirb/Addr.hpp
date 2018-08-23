#ifndef GTIRB_ADDR_H
#define GTIRB_ADDR_H

#include <gtirb/Export.hpp>
#include <cstddef>
#include <cstdint>
#include <utility>

namespace gtirb {
/// A special class to store an Effective Address. It is a thin wrapper around
/// a uint64_t for 64-bit address storage. An Addr cannot store a relative
/// address as it cannot contain a negative number.
class GTIRB_EXPORT_API Addr {
  uint64_t Address{0};

public:
  constexpr Addr() = default;
  constexpr explicit Addr(uint64_t X) : Address(X) {}

  explicit operator uint64_t() const { return Address; }

  // Addr + integral should result in an Addr.
  friend Addr operator+(const Addr& A, uint64_t Offset) {
    return Addr(A.Address + Offset);
  }
  // Addr - integral should result in an Addr.
  friend Addr operator-(const Addr& A, uint64_t Offset) {
    return Addr(A.Address - Offset);
  }

  // Addr - Addr should result in a ptrdiff_t.
  friend ptrdiff_t operator-(const Addr& A, const Addr& B) {
    return static_cast<ptrdiff_t>(A.Address - B.Address);
  }

  // Equality and relational operators; additional operations are provided
  // through std::rel_ops in <utility>.
  friend bool operator==(const Addr& LHS, const Addr& RHS) {
    return LHS.Address == RHS.Address;
  }
  friend bool operator<(const Addr& LHS, const Addr& RHS) {
    return LHS.Address < RHS.Address;
  }
};

/// Exclusive limit of object's address range.
///
/// Object can be any type which specifies a range of addresses via
/// getAddress() and getSize() methods (e.g. DataObject).
template <typename T> Addr addressLimit(const T& Object) {
  return Object.getAddress() + Object.getSize();
}

/// Does object contain a given Addr?
///
/// Object can be any type which specifies a range of addresses via
/// getAddress() and getSize() methods (e.g. DataObject).
template <typename T> bool containsAddr(const T& Object, Addr Ea) {
  using namespace std::rel_ops;
  return Object.getAddress() <= Ea && addressLimit(Object) > Ea;
}
} // namespace gtirb

#endif // GTIRB_ADDR_H
