//===- CFGSerialization.hpp -------------------------------------*- C++ -*-===//
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

#ifndef GTIRB_CFG_SERIALIZATION_HPP
#define GTIRB_CFG_SERIALIZATION_HPP

#include <gtirb/CFG.hpp>

namespace gtirb {
class Context;
namespace proto {
class CFG;
}

/// @cond INTERNAL
/// \ingroup CFG_GROUP
/// \brief Serialize a \ref CFG into a protobuf message.
///
/// \param Cfg   The CFG to serialize.
///
/// \return A protobuf message representing the \ref CFG and its
/// component blocks (\ref Block).
proto::CFG toProtobuf(const CFG& Cfg);

/// \ingroup CFG_GROUP
/// \brief Initialize a \ref CFG from a protobuf message.
///
/// \param      C        The Context in which the deserialized CFG will be held.
/// \param      Message  The protobuf message from which to deserialize.
/// \param[out] Result   The CFG to initialize.
///
/// \return true if the \ref CFG could be deserialized, false otherwise.
bool fromProtobuf(Context& C, CFG& Result, const proto::CFG& Message);
/// @endcond

} // namespace gtirb

#endif // GTIRB_CFG_SERIALIZATION_HPP
