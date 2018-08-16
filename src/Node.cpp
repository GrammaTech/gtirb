#include "Node.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <gsl/gsl>

using namespace gtirb;

std::map<UUID, Node*> Node::UuidMap;

Node* Node::getByUUID(UUID Uuid) {
  auto Found = Node::UuidMap.find(Uuid);
  if (Found != Node::UuidMap.end()) {
    return Found->second;
  } else {
    return nullptr;
  }
}

Node::Node() : Uuid(boost::uuids::random_generator()()) {
  Node::UuidMap[this->Uuid] = this;
}

Node::~Node() noexcept {
  auto Found = Node::UuidMap.find(this->Uuid);
  if (Found != Node::UuidMap.end()) {
    assert(Found->second == this);
    Node::UuidMap.erase(Found);
  }
}

void Node::setUUID() {
  Node::UuidMap.erase(this->Uuid);
  this->Uuid = boost::uuids::random_generator()();
  Node::UuidMap[this->Uuid] = this;
}

void Node::setUUID(UUID X) {
  // UUID should not previously exist
  assert(Node::UuidMap.find(X) == Node::UuidMap.end());

  Node::UuidMap.erase(this->Uuid);
  this->Uuid = X;
  Node::UuidMap.emplace(X, this);
}

UUID Node::getUUID() const { return this->Uuid; }

std::string gtirb::uuidToString(const UUID& Uuid) {
  return boost::uuids::to_string(Uuid);
}

UUID gtirb::uuidFromString(const std::string& Text) {
  return boost::uuids::string_generator()(Text);
}
