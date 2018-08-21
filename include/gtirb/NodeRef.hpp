#pragma once

#include <gtirb/Node.hpp>

namespace gtirb {
///
/// \class NodeRef
///
/// Reference a Node by UUID.
///
/// This behaves like a pointer, but it looks up the node by its UUID, which
/// is persistent across serialization, moves, etc.
template <typename NodeT> class NodeRef {
public:
  NodeRef() = default;
  NodeRef(const UUID& Uuid_) : Uuid(Uuid_) {}
  NodeRef(const NodeT& node) : NodeRef(node.getUUID()){};
  NodeRef(const NodeT* node) : NodeRef(*node) {}
  NodeRef(const NodeRef&) = default;
  NodeRef(NodeRef&&) = default;
  NodeRef& operator=(NodeRef&&) = default;
  NodeRef& operator=(const NodeRef&) = default;

  // Cast to pointer
  operator NodeT*() { return this->get(); }

  // Cast to boolean
  operator bool() const { return this->get(); }

  // Dereference
  const NodeT& operator*() const { return *this->get(); }
  NodeT& operator*() { return *this->get(); }

  const NodeT* operator->() const { return this->get(); }

  NodeT* operator->() { return this->get(); }

  UUID getUUID() const { return this->Uuid; }

private:
  UUID Uuid;

  NodeT* get() const {
    return dyn_cast_or_null<NodeT>(Node::getByUUID(this->Uuid));
  }
};
} // namespace gtirb
