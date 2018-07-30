#pragma once

#include <gtirb/Node.hpp>

namespace gtirb {
template <typename NodeT> class NodeRef {
public:
  NodeRef() = default;
  NodeRef(UUID uuid_) : uuid(uuid_) {}
  NodeRef(const NodeT& node) : uuid(node.getUUID()){};
  NodeRef(const NodeRef&) = default;
  NodeRef(NodeRef&&) = default;
  NodeRef& operator=(NodeRef&& data) = default;
  NodeRef& operator=(const NodeRef& data) = default;

  // Cast to pointer
  operator NodeT*() { return this->get(); }

  // Cast to boolean
  operator bool() const { return this->get(); }

  // Dereference
  const NodeT& operator*() const { return *this->get(); }
  NodeT& operator*() { return *this->get(); }

  const NodeT* operator->() const { return this->get(); }

  NodeT* operator->() { return this->get(); }

  UUID getUUID() const { return this->uuid; }

private:
  UUID uuid;

  NodeT* get() const { return dynamic_cast<NodeT*>(Node::getByUUID(this->uuid)); }
};
} // namespace gtirb
