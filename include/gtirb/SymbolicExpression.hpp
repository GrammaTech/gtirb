//===- SymbolicExpression.hpp -----------------------------------*- C++ -*-===//
//
//  Copyright (C) 2018-2019 GrammaTech, Inc.
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
#ifndef GTIRB_SYMBOLICEXPRESSION_H
#define GTIRB_SYMBOLICEXPRESSION_H

#include <gtirb/Addr.hpp>
#include <gtirb/Symbol.hpp>
#include <cstdint>
#include <functional>
#include <map>
#include <string>
#include <variant>

/// \file SymbolicExpression.hpp
/// \ingroup SYMBOLIC_EXPRESSION_GROUP
/// \brief Types and operations for symbolic expressions.
///
/// \see \ref SYMBOLIC_EXPRESSION_GROUP.
namespace proto {
class SymbolicExpression;
}
namespace gtirb {
class Context;

/// \defgroup SYMBOLIC_EXPRESSION_GROUP Symbolic Expressions and Operands
/// \brief Represent data values or instruction operands which
/// should be intepreted as referring to symbols.
/// @{

/// \brief Represents a
/// \ref SYMBOLIC_EXPRESSION_GROUP "symbolic operand" of the form
/// "Sym + Offset", representing an offset from a stack variable.
struct SymStackConst {
  int Offset;  ///< Constant offset.
  Symbol* Sym; ///< Symbol representing a stack variable.

  friend bool operator==(const SymStackConst& LHS, const SymStackConst& RHS) {
    return LHS.Offset == RHS.Offset && LHS.Sym == RHS.Sym;
  }

  friend bool operator!=(const SymStackConst& LHS, const SymStackConst& RHS) {
    return !operator==(LHS, RHS);
  }
};

/// \brief Represents a
/// \ref SYMBOLIC_EXPRESSION_GROUP "symbolic operand" of the form
/// "Sym + Offset".
struct SymAddrConst {
  int64_t Offset; ///< Constant offset.
  Symbol* Sym;    ///< Symbol representing an address.

  friend bool operator==(const SymAddrConst& LHS, const SymAddrConst& RHS) {
    return LHS.Offset == RHS.Offset && LHS.Sym == RHS.Sym;
  }

  friend bool operator!=(const SymAddrConst& LHS, const SymAddrConst& RHS) {
    return !operator==(LHS, RHS);
  }
};

/// \brief Represents a
/// \ref SYMBOLIC_EXPRESSION_GROUP "symbolic operand" of the form
/// "(Sym1 - Sym2) / Scale + Offset"
struct SymAddrAddr {
  int64_t Scale;  ///< Constant scale factor.
  int64_t Offset; ///< Constant offset.
  Symbol* Sym1;   ///< Symbol representing the base address.
  Symbol* Sym2;   ///< Symbol to subtract from \p Sym1.

  friend bool operator==(const SymAddrAddr& LHS, const SymAddrAddr& RHS) {
    return LHS.Scale == RHS.Scale && LHS.Offset == RHS.Offset &&
           LHS.Sym1 == RHS.Sym1 && LHS.Sym2 == RHS.Sym2;
  }

  friend bool operator!=(const SymAddrAddr& LHS, const SymAddrAddr& RHS) {
    return !operator==(LHS, RHS);
  }
};

/// \brief A \ref SYMBOLIC_EXPRESSION_GROUP "symbolic expression".
using SymbolicExpression =
    std::variant<SymStackConst, SymAddrConst, SymAddrAddr>;

/// @cond INTERNAL
/// \brief Initialize a SymbolicExpression from a protobuf message.
///
/// \param      C        The Context in which the deserialized
///                      SymbolicExpression will be held.
/// \param      Message  The protobuf message from which to deserialize.
/// \param[out] Result   The SymbolicExpression to initialize.
///
/// \return void
GTIRB_EXPORT_API void fromProtobuf(Context& C, SymbolicExpression& Result,
                                   const proto::SymbolicExpression& Message);

/// \brief Serialize a SymbolicExpression into a protobuf message.
///
/// \param Value   The SymbolicExpression to serialize.
///
/// \return A protobuf message representing the SymbolicExpression.
GTIRB_EXPORT_API proto::SymbolicExpression
toProtobuf(const SymbolicExpression& Value);

/// @endcond
/// @}
// (end \defgroup SYMBOLIC_EXPRESSION_GROUP)

} // namespace gtirb

namespace std {
template <> struct hash<gtirb::SymStackConst> {
  typedef gtirb::SymStackConst argument_type;
  typedef std::size_t result_type;

  result_type operator()(const argument_type& Obj) const noexcept {
    const result_type Off = std::hash<int>{}(Obj.Offset);
    const result_type P = std::hash<gtirb::Symbol*>{}(Obj.Sym);
    return Off ^ (P << 1);
  }
};

template <> struct hash<gtirb::SymAddrConst> {
  typedef gtirb::SymAddrConst argument_type;
  typedef std::size_t result_type;

  result_type operator()(const argument_type& Obj) const noexcept {
    const result_type Off = std::hash<int64_t>{}(Obj.Offset);
    const result_type P = std::hash<gtirb::Symbol*>{}(Obj.Sym);
    return Off ^ (P << 1);
  }
};

template <> struct hash<gtirb::SymAddrAddr> {
  typedef gtirb::SymAddrAddr argument_type;
  typedef std::size_t result_type;

  result_type operator()(const argument_type& Obj) const noexcept {
    result_type S = std::hash<int64_t>{}(Obj.Scale);
    comb(S, std::hash<int64_t>{}(Obj.Offset));
    comb(S, std::hash<gtirb::Symbol*>{}(Obj.Sym1));
    comb(S, std::hash<gtirb::Symbol*>{}(Obj.Sym2));
    return S;
  }

private:
  void comb(result_type& One, result_type Two) const noexcept {
    One ^= Two + 0x9e3779b9 + (One << 6) + (One >> 2);
  }
};
} // namespace std

#endif // GTIRB_SYMBOLICEXPRESSION_H
