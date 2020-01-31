//===- ExtensibleEnum.hpp ---------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2020 GrammaTech, Inc.
//
//  This code is licensed under the MIT license. See the LICENSE file in the
//  project root for license terms.
//
//  This project is sponsored by the Office of Naval Research, One Liberty
//  Center, 875 N. Randolph Street, Arlington, VA 22203 under contract #
//  N68335-17-C-0700.  The content of the information does not necessarily
//  reflect the position or policy of the Government and no official
//  endorsement should be inferred.
//
//===----------------------------------------------------------------------===//
#ifndef GTIRB_EXTENSIBLEENUM_H
#define GTIRB_EXTENSIBLEENUM_H

namespace gtirb {

// Base class is templated on the underlying enum that is being made
// extensible. This helps ensure type safety so that it is harder to
// compare enumerators from distinct enumerations, and it also allows
// us to store values using the same representation as the enumeration
// uses.
template <typename UnderlyingEnum> class ExtensibleEnum {
public:
  using type = std::underlying_type_t<UnderlyingEnum>;

private:
  type Value{};

public:
  // It is purposeful that the constructor is not explicit; this allows
  // implicit conversions such as the ones used by the initialization of the
  // enumerators listed in the specific enumeration subclasses.
  constexpr ExtensibleEnum(type V) : Value(V) {}

  // Defaulting the constructor to ensure that the enumeration is a regular
  // type.
  constexpr ExtensibleEnum() = default;

  // It is purposeful that the conversion operator is explicit; this prevents
  // accidental implicit conversion to the underlying type, adding extra type
  // safety for the subclasses. Without this, you could compare two different
  // enumerations because they would implicitly convert to their underlying
  // integral types.
  explicit constexpr operator type() const { return Value; }
  constexpr operator UnderlyingEnum() const {
    return static_cast<UnderlyingEnum>(Value);
  }

  friend constexpr bool operator<(const ExtensibleEnum& LHS,
                                  const ExtensibleEnum& RHS) {
    return LHS.Value < RHS.Value;
  }

  friend constexpr bool operator>(const ExtensibleEnum& LHS,
                                  const ExtensibleEnum& RHS) {
    return LHS.Value > RHS.Value;
  }

  friend constexpr bool operator==(const ExtensibleEnum& LHS,
                                   const ExtensibleEnum& RHS) {
    return LHS.Value == RHS.Value;
  }

  friend constexpr bool operator!=(const ExtensibleEnum& LHS,
                                   const ExtensibleEnum& RHS) {
    return LHS.Value != RHS.Value;
  }

  friend constexpr bool operator<=(const ExtensibleEnum& LHS,
                                   const ExtensibleEnum& RHS) {
    return LHS.Value <= RHS.Value;
  }
  friend constexpr bool operator>=(const ExtensibleEnum& LHS,
                                   const ExtensibleEnum& RHS) {
    return LHS.Value >= RHS.Value;
  }
};

} // namespace gtirb
#endif // GTIRB_EXTENSIBLEENUM_H
