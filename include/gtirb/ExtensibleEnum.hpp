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

/// \file ExtensibleEnum.hpp
/// \brief Defines an enumeration-like type that allows the enumeration to be
/// extended by others.

namespace gtirb {
/// \brief Due to GTIRB's use as an intermediary representation for exchange
/// between processes, we need some way in which to handle extending
/// enumerations more gracefully than what C++ allows by default. Ideally, we
/// wish we could do something like:
///
/// // In GTIRB proper:
/// enum class FileFormats {
///   Undefined,
///   COFF,
///   ELF,
/// };
///
/// // In an extension to GTIRB:
/// enum class ExtFileFormats : public FileFormats {
///   PE32,
///   Macho,
/// };
///
/// Unfortunately, C++ does not allow inheritance between enumerations. Instead,
/// we provide the ExtensibleEnum class as a way to model a scoped enumeration.
///
/// To define your own extensible enumeration, you should subclass the
/// ExtensibleEnum class to hold the enumerators you wish to expose. You will
/// have to expose the underlying constructors with a using declaration, and you
/// have to take care to define the enumerators in a way that allows them to be
/// used within a constexpr context without violating the ODR. A simple example
/// is:
///
/// class MyExtensibleEnum : ExtensibleEnum<> { // Underlying type is int
/// public:
///   using ExtensibleEnum::ExtensibleEnum; // Inherit base class constructors
///
///   // Declare the enumerators as static const members of the class type.
///   // Note that you cannot declare them as constexpr because the type is
///   // incomplete until reaching the closing brace.
///   static const MyExtensibleEnum EnumeratorOne;
///   static const MyExtensibleEnum EnumeratorTwo;
/// };
/// // Define the enumerators as inline constexpr members of the class type.
/// // This ensures that the enumerators can be used in a constexpr context and
/// // do not violate the ODR. Note that you must use a braced-init-list to
/// // initialize the value from an integral type (this also helps to ensure
/// // that there is no accidental narrowing conversion between the value and
/// // underlying type of the extensible enumeration).
/// inline constexpr MyExtensibleEnum MyExtensibleEnum::EnumeratorOne{12};
/// inline constexpr MyExtensibleEnum MyExtensibleEnum::EnumeratorTwo{42};
///
/// To define an extension to an existing extensible enumeration, you shuold
/// subclass the extensible enumeration base class and add additional enumerator
/// members to it. This also requires introducing the base class constructors,
/// as well as an additional copy constructor so that the derived class can
/// properly interopate with objects of the base class type. A simple example
/// is:
///
/// class MyExtendedEnum : MyExtensibleEnum {
/// public:
///
///   using MyExtensibleEnum::MyExtensibleEnum; // Inherit base class ctors
///   constexpr MyExtendedEnum(const MyExtensibleEnum& Other)
///       : MyExtensibleEnum(Other) {}
///
///   static const MyExtendedEnum EnumeratorThree;
/// };
/// inline constexpr MyExtendedEnum MyExtendedEnum::EnumeratorThree{3};
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
