#pragma once

#include <gtirb/Node.hpp>

namespace gtirb {
template <typename NodeT> class NodeReference {
public:
  NodeReference() = default;
  NodeReference(UUID uuid_) : uuid(uuid_) {}
  NodeReference(const NodeT& node) : uuid(node.getUUID()){};
  NodeReference(const NodeReference&) = default;
  NodeReference(NodeReference&&) = default;
  NodeReference& operator=(NodeReference&& data) = default;
  NodeReference& operator=(const NodeReference& data) = default;

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
}
