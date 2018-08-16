#include "Node.hpp"
// FIXME: SymbolicExpression.hpp must be included before including Module.hpp
// due to a bug with our header include orders. This should be fixed before we
// release GTIRB to the public.
#include "gtirb/SymbolicExpression.hpp"
#include "gtirb/DataObject.hpp"
#include "gtirb/ImageByteMap.hpp"
#include "gtirb/IR.hpp"
#include "gtirb/Module.hpp"
#include "gtirb/Section.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>

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

Node::Node(Kind Knd) : K(Knd), Uuid(boost::uuids::random_generator()()) {
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

void GTIRB_EXPORT_API details::ClearUUIDs(Node *N) {
  N->setUUID();

  if (auto *I = dyn_cast<IR>(N)) {
    for (auto *M : I->getModules()) {
      details::ClearUUIDs(M);
    }
  } else if (auto *M = dyn_cast<Module>(N)) {
    details::ClearUUIDs(&M->getImageByteMap());
    for (auto *D : M->getData()) {
      details::ClearUUIDs(D);
    }
    for (auto *S : M->getSections()) {
      details::ClearUUIDs(S);
    }
    for (auto &S : M->getSymbols()) {
      details::ClearUUIDs(S.second);
    }
    for (auto &B : blocks(M->getCFG())) {
      details::ClearUUIDs(&B);
    }
  }
}
