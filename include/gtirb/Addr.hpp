#ifndef GTIRB_ADDR_H
#define GTIRB_ADDR_H

#include <gtirb/Export.hpp>
#include <cstddef>
#include <cstdint>

namespace gtirb {
/// \brief A special class to store an Effective Address.
///
/// It is a thin wrapper around
/// a uint64_t for 64-bit address storage. An Addr cannot store a relative
/// address as it cannot contain a negative number.
class GTIRB_EXPORT_API Addr {
  uint64_t Address{0};

public:
  /// \brief Default constructor.
  /// The default implementation provided by the compiler is used.
  ///
  constexpr Addr() = default;


  /// \brief Explicit conversion from \c uint64_t to \ref Addr.
  ///
  /// \param X The address.
  ///
  constexpr explicit Addr(uint64_t X) : Address(X) {}


  /// \brief Explicitly convert \ref Addr to \c uint64_t.
  ///
  /// \return An integer representation of the \ref Addr.
  ///
  explicit operator uint64_t() const { return Address; }


  /// \brief Preincrement for \ref Addr.
  ///
  /// \return The incremented \ref Addr.
  ///
  /// DOCFIXME[what happens if represented address == max?]
  Addr& operator++() {
    ++Address;
    return *this;
  }


  /// \brief Postincrement for \ref Addr.
  ///
  /// \return A new \ref Addr representing the incremented address.
  ///
  ///
  /// DOCFIXME[what happens if represented address == max?]
  Addr operator++(int) {
    Addr R(*this);
    ++Address;
    return R;
  }


  /// \brief Predecrement for \ref Addr.
  ///
  /// \return The decremented \ref Addr.
  ///
  ///
  /// DOCFIXME[what happens if represented address == 0?]
  Addr& operator--() {
    --Address;
    return *this;
  }


  /// \brief Postdecrement for \ref Addr.
  /// 
  /// \return A new \ref Addr representing the decremented address.
  ///
  /// DOCFIXME[what happens if represented address == 0?]
  Addr operator--(int) {
    Addr R(*this);
    --Address;
    return R;
  }


  /// \brief Binary + operator for \ref Addr + integral offset.
  /// 
  /// \param A        The \ref Addr operand to +.
  ///
  /// \param Offset   The offset to add to \p A.
  ///
  /// \return A new \ref Addr representing \p A + \p Offset.
  ///
  friend Addr operator+(const Addr& A, uint64_t Offset) {
    return Addr(A.Address + Offset);
  }


  /// \brief Add-assign for \ref Addr.
  /// 
  /// \param Offset   The offset to add to the represented address.
  ///
  /// \return \c *this 
  ///
  /// DOCFIXME[what happens if represented address == max?]
  Addr& operator+=(uint64_t Offset) {
    Address += Offset;
    return *this;
  }


  /// \brief Binary - operator for \ref Addr - integral offset.
  /// 
  /// \param A        The \ref Addr operand to -.
  ///
  /// \param Offset   The offset to subtract from \p A.
  ///
  /// \return A new \ref Addr representing  \p A - \p Offset.
  ///
  /// DOCFIXME[what happens if offset > the represented address?]
  friend Addr operator-(const Addr& A, uint64_t Offset) {
    return Addr(A.Address - Offset);
  }


  /// \brief Subtract-assign for \ref Addr.
  /// 
  /// \param Offset   The offset to subtract from the represented address.
  ///
  /// \return \c *this 
  ///
  /// DOCFIXME[what happens if offset > the represented address?]
  Addr& operator-=(uint64_t Offset) {
    Address -= Offset;
    return *this;
  }

  /// \brief Binary - operator for \ref Addr - \ref Addr.
  /// 
  /// \param A        The minuend.
  ///
  /// \param B        The subtrahend.
  ///
  /// \return         The \c int64_t difference \p A - \p B.
  ///
  /// DOCFIXME[what happens if offset > the represented address?]
  friend int64_t operator-(const Addr& A, const Addr& B) {
    return static_cast<int64_t>(A.Address - B.Address);
  }

  /// \brief Equality operator for \ref Addr.
  friend bool operator==(const Addr& LHS, const Addr& RHS) {
    return LHS.Address == RHS.Address;
  }

  /// \brief Inquality operator for \ref Addr.
  friend bool operator!=(const Addr& LHS, const Addr& RHS) {
    return !operator==(LHS, RHS);
  }

  /// \brief Less-than operator for \ref Addr.
  friend bool operator<(const Addr& LHS, const Addr& RHS) {
    return LHS.Address < RHS.Address;
  }

  /// \brief Greater-than operator for \ref Addr.
  friend bool operator>(const Addr& LHS, const Addr& RHS) {
    return operator<(RHS, LHS);
  }

  /// \brief Less-than-or-equal operator for \ref Addr.
  friend bool operator<=(const Addr& LHS, const Addr& RHS) {
    return !operator<(RHS, LHS);
  }

  /// \brief Greater-than-or-equal operator for \ref Addr.
  friend bool operator>=(const Addr& LHS, const Addr& RHS) {
    return !operator<(LHS, RHS);
  }
};


/// DOCFIXME[check all]
/// \brief Exclusive upper limit of object's address range.
///
/// \param Object     DOCFIXME
///
/// \tparam T         Any type that specifies a range of 
///                   addresses via getAddress() and getSize()
///                   methods (e.g. DataObject).
///
/// \return An address (\ref Addr) A such that A-1 is in \p Object and
/// A is not.
///
/// Object can be any type which specifies a range of addresses via
/// getAddress() and getSize() methods (e.g. DataObject).
template <typename T> Addr addressLimit(const T& Object) {
  return Object.getAddress() + Object.getSize();
}


/// DOCFIXME[check all]
/// \brief Check: Does the specified object contain the specified address?
///
/// \param Object  The object of interest.
///
/// \param Ea      The address of interest.
///
/// \tparam T      Any type that specifies a range of 
///                addresses via getAddress() and getSize()
///                methods (e.g. DataObject).
///
/// \return \c true if \p Ea is in the address range of \p Object, \c
/// false otherwise.
///
/// Object can be any type which specifies a range of addresses via
/// getAddress() and getSize() methods (e.g. DataObject).
template <typename T> bool containsAddr(const T& Object, Addr Ea) {
  return Object.getAddress() <= Ea && addressLimit(Object) > Ea;
}

} // namespace gtirb

#endif // GTIRB_ADDR_H
