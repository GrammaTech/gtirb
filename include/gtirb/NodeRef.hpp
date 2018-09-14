//===- NodeRef.hpp ----------------------------------------------*- C++ -*-===//
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
#ifndef GTIRB_NODEREF_H
#define GTIRB_NODEREF_H

#include <gtirb/Node.hpp>

/// \file NodeRef.hpp
/// \brief Class gtirb::NodeRef.

namespace gtirb {

/// \class NodeRef
///
/// \brief Reference a Node by UUID.
///
/// \tparam NodeT DOCFIXME
///
/// This behaves like a pointer, but it looks up the node by its UUID, which
/// is persistent across serialization, moves, etc.
template <typename NodeT> class NodeRef {
public:
  /// \brief Default constructor.
  NodeRef() = default;

  /// \brief Constructor.
  ///
  /// \param Uuid_ The UUID to reference DOCFIXME[check].
  ///
  NodeRef(const UUID& Uuid_) : Uuid(Uuid_) {}

  /// \brief Constructor
  ///
  /// \param node The Node to reference DOCFIXME[check].
  ///
  NodeRef(const NodeT& node) : Uuid(node.getUUID()){};

  NodeRef(const NodeT* node) : NodeRef(*node) {}

  /// \brief Copy constructor.
  NodeRef(const NodeRef&) = default;

  /// \brief Move constructor
  NodeRef(NodeRef&&) = default;

  /// \brief Move assignment operator overload
  NodeRef& operator=(NodeRef&&) = default;

  /// \brief Assignment operator overload.
  NodeRef& operator=(const NodeRef&) = default;

  const NodeT* get(Context& C) const {
    return dyn_cast_or_null<NodeT>(Node::getByUUID(C, this->Uuid));
  }
  NodeT* get(Context& C) {
    return dyn_cast_or_null<NodeT>(Node::getByUUID(C, this->Uuid));
  }

  const UUID& getUUID() const { return this->Uuid; }

private:
  UUID Uuid;
};
} // namespace gtirb

#endif // GTIRB_NODEREF_H
