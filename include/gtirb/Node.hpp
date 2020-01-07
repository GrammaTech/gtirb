//===- Node.hpp -------------------------------------------------*- C++ -*-===//
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
#ifndef GTIRB_NODE_H
#define GTIRB_NODE_H

#include <gtirb/Casting.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/Export.hpp>
#include <functional>
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

  // The enum constants below must be grouped according to the inheritance
  // hierarchy such that all descendants of a type X must appear between the
  // constant for X and LAST_X. This allows us to quickly check whether a type
  // is a descendant of X by checking if the enum constant falls in that range.
  enum class Kind {
    Node,
    CfgNode,
    CodeBlock,
    ProxyBlock,
    LAST_CfgNode = ProxyBlock, // Mark last descendant of CfgNode
    DataBlock,
    IR,
    Module,
    Section,
    Symbol,
    ByteInterval,
    LAST_Node = ByteInterval, // Mark last descendant of Node
  };
  /// \endcond

  /// \brief Retrieve a node by its UUID.
  ///
  /// \return The Node with the given UUID, or nullptr if none exists.
  static Node* getByUUID(Context& C, const UUID& Uuid) {
    return C.findNode(Uuid);
  }

  /// \brief Retrieve a node by its UUID.
  ///
  /// \return The Node with the given UUID, or nullptr if none exists.
  static const Node* getByUUID(const Context& C, const UUID& Uuid) {
    return C.findNode(Uuid);
  }

  /// \brief Create a Node object in its default state.
  ///
  /// \param C  The Context in which this object will be held.
  ///
  /// \return The newly created object.
  static Node* Create(Context& C) { return C.Create<Node>(C, Kind::Node); }

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
  static bool classof(const Node* N) { return classofKind(N->getKind()); }
  static bool classofKind(Kind K) {
    return K >= Kind::Node && K <= Kind::LAST_Node;
  }
  /// \endcond

protected:
  /// \cond INTERNAL
  Node(Context& C, Kind Knd);

  /// \brief Add a new \ref Node to a \ref Module or \ref IR's lookup indices.
  ///
  /// The module has indices for fast lookup of certain traits. When a node gets
  /// added somewhere in this module, these indices need to be updated. Call
  /// this function to update these indices. If getModule on this node returns
  /// null, the function does nothing and returns, so ensure that the node has
  /// parentage to the module before you call this function.
  ///
  /// Valid \ref Node types to call this function from include \ref
  /// ByteInterval, \ref CodeBlock, \ref DataBlock, \ref Section, \ref
  /// Symbol, and \ref Module.
  void addToIndices();

  /// \brief Update the lookup indices of a Module or IR when a \ref Node
  /// changes.
  ///
  /// The module has indices for fast lookup of certain traits. When mutating
  /// these traits, call this function, with your mutation code in a lambda.
  /// if getModule on this node returns null, then no update is performed, but
  /// the lambda is still called.
  ///
  /// Valid \ref Node types to call this function from include \ref
  /// ByteInterval, \ref CodeBlock, \ref DataBlock, \ref Section, \ref
  /// Symbol, and \ref Module.
  ///
  /// TODO: make it templated so it can use any Callable rather than a
  /// std::function; this will improve performance, but to do that, we have
  /// a dependency knot to untangle.
  ///
  /// \param F  A function taking no arguments and retuning void. This function
  /// should mutate N.
  void mutateIndices(const std::function<void()>& F);

  /// \brief Remove a \ref Node from a \ref Module or \ref IR's lookup
  /// indices.
  ///
  /// The module has indices for fast lookup of certain traits. When a node gets
  /// removed somewhere in this module, these indices need to be updated. Call
  /// this function to update these indices. If getModule on this node returns
  /// null, the function does nothing and returns, so ensure that the node still
  /// has parentage to the module before you call this function.
  ///
  /// Valid \ref Node types to call this function from include \ref
  /// ByteInterval, \ref CodeBlock, \ref DataBlock, \ref Section, \ref
  /// Symbol, and \ref Module.
  void removeFromIndices();
  /// \endcond

private:
  Kind K;
  UUID Uuid;
  // The Context object can never be null as it can only be passed to the Node
  // constructor by reference. However, we don't want to store a reference to
  // the Context object because we want to keep the Node class copyable and
  // Context needs to own a move-only allocator.
  Context* Ctx;

  // Assign a new UUID to this node. This is only needed when deserializing
  // objects, as there are no constructors allowing the user to set the UUID on
  // construction.
  void setUUID(UUID X);
  friend void setNodeUUIDFromBytes(Node* Node, const std::string& Bytes);

  friend class Context;
};

} // namespace gtirb

#endif // GTIRB_NODE_H
