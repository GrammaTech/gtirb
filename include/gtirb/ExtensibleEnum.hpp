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

#include <gtirb/Export.hpp>
#include <initializer_list>
#include <limits>
#include <type_traits>

namespace gtirb {

// Due to GTIRB's use as an intermediary representation for exchange between
// processes, we need some way in which to handle extending enumerations more
// gracefully than what C++ allows by default. Ideally, we wish we could do
// something like:
//
// // In GTIRB proper:
// enum class FileFormats {
//   Undefined,
//   COFF,
//   ELF,
// };
//
// // In an extension to GTIRB:
// enum class ExtFileFormats : public FileFormats {
//   PE32,
//   Macho,
// };
//
// Unfortunately, C++ does not allow inheritance between enumerations. Instead,
// we provide the ExtensibleEnum class as a way to model a scoped enumeration.

// TODO: make ExtensibleEnum model an enum class, verify behavior against
// enum classes, compile-time tests with static_assert, should be sort of
// like the Addr class in terms of design principles.

template <typename UnderlyingType = int> class GTIRB_EXPORT_API ExtensibleEnum {
  UnderlyingType Value;

public:
  ExtensibleEnum() = default;
  explicit constexpr ExtensibleEnum(UnderlyingType V) : Value(V) {}

  template <typename IntegralOrEnumTy,
            typename = std::enable_if_t<std::is_integral_v<IntegralOrEnumTy> ||
                                        std::is_enum_v<IntegralOrEnumTy>>>
  explicit constexpr operator IntegralOrEnumTy() const {
    return static_cast<IntegralOrEnumTy>(Value);
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
