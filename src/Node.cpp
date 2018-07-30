#include "Node.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <gsl/gsl>

using namespace gtirb;

std::map<UUID, Node*> Node::uuidMap;

Node* Node::getByUUID(UUID uuid) {
  auto found = Node::uuidMap.find(uuid);
  if (found != Node::uuidMap.end()) {
    return found->second;
  } else {
    return nullptr;
  }
}

// UUID construction is a bottleneck in the creation of Node.  (~0.5ms)
Node::Node() : uuid(boost::uuids::random_generator()()) { Node::uuidMap[this->uuid] = this; }

Node::Node(const Node& other) { this->setUUID(); }

Node::Node(Node&& other) noexcept : uuid(std::move(other.uuid)) {
  other.uuid = UUID();
  Node::uuidMap[this->uuid] = this;
}

Node& Node::operator=(Node&& other) noexcept {
  // Update UUID map
  auto found = Node::uuidMap.find(other.uuid);
  assert(found != Node::uuidMap.end());
  this->uuid = other.uuid;
  found->second = this;
  other.uuid = UUID();

  return *this;
}

Node::~Node() noexcept {
  auto found = Node::uuidMap.find(this->uuid);
  if (found != Node::uuidMap.end()) {
    assert(found->second == this);
    Node::uuidMap.erase(found);
  }
}

void Node::setUUID() {
  Node::uuidMap.erase(this->uuid);
  this->uuid = boost::uuids::random_generator()();
  Node::uuidMap[this->uuid] = this;
}

void Node::setUUID(UUID x) {
  // UUID should not previously exist
  assert(Node::uuidMap.find(x) == Node::uuidMap.end());

  Node::uuidMap.erase(this->uuid);
  this->uuid = x;
  Node::uuidMap.emplace(x, this);
}

UUID Node::getUUID() const { return this->uuid; }
