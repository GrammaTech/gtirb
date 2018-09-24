//===- Node.hpp -------------------------------------------------*- C++ -*-===//
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
#ifndef GTIRB_NODE_H
#define GTIRB_NODE_H

#include <gtirb/Casting.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/Export.hpp>
#include <gsl/gsl>
#include <string>

/// \file Node.hpp
/// \brief Class gtirb::Node.

namespace gtirb {
class Node;

/// \class Node
///
/// \brief Represents the base of the Node class hierarchy.
///
/// Objects of Node types can be converted into more specific types by
/// using the \ref casting "casting machinery" from Casting.hpp. You
/// can use static_cast<>() and reinterpret_cast<>(), but cast<>() and
/// dyn_cast<>() are safer alternatives. You cannot use dynamic_cast<>
/// to cast Node objects.
class GTIRB_EXPORT_API Node {
public:
  /// \cond internal
  enum class Kind {
    Node,
    Block,
    DataObject,
    ImageByteMap,
    IR,
    Module,
    Section,
    Symbol,
  };
  /// \endcond

  /// \brief Retrieve a node by its UUID.
  ///
  /// \return The Node with the given UUID, or nullptr if none exists.
  static Node* getByUUID(Context& C, const UUID& Uuid) {
    return C.findNode(Uuid);
  }

  /// \brief Create a Node object in its default state.
  ///
  /// \param C  The Context in which this object will be held.
  ///
  /// \return The newly created object.
  static Node* Create(Context& C) { return new (C) Node(C, Kind::Node); }

  /// \brief Cleans up resources no longer needed by the Node object.
  ~Node() noexcept;

  /// \brief Get the Universally Unique ID (UUID) for \c this.
  ///
  /// \return The UUID.
  const UUID& getUUID() const { return Uuid; }

  /// \cond INTERNAL
  Kind getKind() const { return K; }
  /// \endcond

  /// \cond INTERNAL
  static bool classof(const Node* N) { return N->getKind() == Kind::Node; }
  /// \endcond

protected:
  /// \cond INTERNAL
  Node(Context& C, Kind K);
  /// \endcond

private:
  Kind K;
  UUID Uuid;
  // The Context object can never be null as it can only be passed to the Node
  // constructor by reference. However, we don't want to store a reference to
  // the Context object because we want to keep the Node class copyable and
  // Context needs to own a move-only allocator.
  gsl::not_null<Context*> Ctx;

  // Assign a new UUID to this node. This is only needed when deserializing
  // objects, as there are no constructors allowing the user to set the UUID on
  // construction.
  void setUUID(UUID X);
  friend void setNodeUUIDFromBytes(Node* Node, const std::string& Bytes);
};

} // namespace gtirb

#endif // GTIRB_NODE_H
