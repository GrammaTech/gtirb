//===- CfgNode.hpp -----------------------------------------------*- C++-*-===//
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
#ifndef GTIRB_CFG_NODE_HPP
#define GTIRB_CFG_NODE_HPP

#include <gtirb/Node.hpp>

/// \file CfgNode.hpp
/// \ingroup CFG_GROUP
/// \brief Base class for nodes of the CFG.
/// \see CFG_GROUP

namespace gtirb {

/// \class CfgNode
///
/// \brief Represents the base of types that can be inserted into the CFG.
class GTIRB_EXPORT_API CfgNode : public Node {
public:
  /// \cond INTERNAL
  static bool classof(const Node* N) { return classofKind(N->getKind()); }
  static bool classofKind(Kind K) {
    return K >= Kind::CfgNode && K <= Kind::LAST_CfgNode;
  }
  /// \endcond
protected:
  CfgNode(Context& C, Kind Knd) : Node(C, Knd) {}
  CfgNode(Context& C, Kind Knd, const UUID& Uuid) : Node(C, Knd, Uuid) {}
};

} // namespace gtirb
#endif // GTIRB_CFG_NODE_HPP
