//===- SymbolicExpression.hpp -----------------------------------*- C++ -*-===//
//
//  Copyright (C) 2018 GrammaTech, Inc.
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
#include <gtirb/NodeRef.hpp>
#include <gtirb/Symbol.hpp>
#include <cstdint>
#include <map>
#include <string>
#include <variant>

/// \file SymbolicExpression.hpp
/// \brief \ref SYMBOLIC_EXPRESSION_GROUP.

namespace proto {
class SymbolicExpression;
}
namespace gtirb {
class Context;

/// \defgroup SYMBOLIC_EXPRESSION_GROUP Symbolic Expressions and Operands
/// \brief DOCFIXME
/// @{

/// \brief Represents a symbolic operand of the form "StackVar + Const".
/// DOCFIXME[word in terms of the field names]
struct SymStackConst {
  // TODO: What's this?
  bool Negate;         ///< DOCFIXME
  int Offset;          ///< DOCFIXME
  int Displacement;    ///< DOCFIXME
  NodeRef<Symbol> Sym; ///< DOCFIXME
};

/// \brief Represents a symbolic operand of the form "Addr + Const".
/// DOCFIXME[word in terms of the field names]
struct SymAddrConst {
  int64_t Displacement; ///< DOCFIXME
  NodeRef<Symbol> Sym;  ///< DOCFIXME
};

/// \brief Represents a symbolic operand of the form "(Addr - Addr) /
/// Scale + Offset"
/// DOCFIXME[word in terms of the field names]
struct SymAddrAddr {
  int64_t Scale;        ///< DOCFIXME
  int64_t Offset;       ///< DOCFIXME
  NodeRef<Symbol> Sym1; ///< DOCFIXME
  NodeRef<Symbol> Sym2; ///< DOCFIXME
};

/// \brief DOCFIXME
using SymbolicExpression =
    std::variant<SymStackConst, SymAddrConst, SymAddrAddr>;

/// \brief DOCFIXME
///
/// \param <unnamed>  Not used.
/// \param  Result    DOCFIXME
/// \param  Message   DOCFIXME
///
/// \return void
GTIRB_EXPORT_API void fromProtobuf(Context&, SymbolicExpression& Result,
                                   const proto::SymbolicExpression& Message);

/// \brief DOCFIXME
///
/// \param  Expr   DOCFIXME
///
/// \return DOCFIXME
GTIRB_EXPORT_API proto::SymbolicExpression
toProtobuf(const SymbolicExpression& Expr);

/// @}
// (end \defgroup SYMBOLIC_EXPRESSION_GROUP)

} // namespace gtirb

#endif // GTIRB_SYMBOLICEXPRESSION_H
