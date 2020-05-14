//===- SymbolicExpressionSerialization.hpp ----------------------*- C++ -*-===//
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

#ifndef GTIRB_SYMBOLIC_EXPRESSION_SERIALIZATION_HPP
#define GTIRB_SYMBOLIC_EXPRESSION_SERIALIZATION_HPP

#include <gtirb/Context.hpp>
#include <gtirb/SymbolicExpression.hpp>

namespace gtirb {
namespace proto {
class SymbolicExpression;
}

/// @cond INTERNAL
/// \brief Initialize a SymbolicExpression from a protobuf message.
///
/// \param      C        The Context in which the deserialized
///                      SymbolicExpression will be held.
/// \param      Message  The protobuf message from which to deserialize.
/// \param[out] Result   The SymbolicExpression to initialize.
///
/// \return true if the expression could be deserialized from protobuf, false
/// otherwise.
bool fromProtobuf(Context& C, SymbolicExpression& Result,
                  const proto::SymbolicExpression& Message);

/// \brief Serialize a SymbolicExpression into a protobuf message.
///
/// \param Value   The SymbolicExpression to serialize.
///
/// \return A protobuf message representing the SymbolicExpression.
proto::SymbolicExpression toProtobuf(const SymbolicExpression& Value);

/// @endcond

} // namespace gtirb

#endif // GTIRB_SYMBOLIC_EXPRESSION_SERIALIZATION_HPP
