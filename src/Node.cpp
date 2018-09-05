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

Node::Node(Context& C, Kind Knd)
    : K(Knd), Uuid(boost::uuids::random_generator()()), Ctx(&C) {
  Ctx->registerNode(Uuid, this);
}

Node::~Node() noexcept {
  Ctx->unregisterNode(this);
}

void Node::setUUID() {
  setUUID(boost::uuids::random_generator()());
}

void Node::setUUID(UUID X) {
  // UUID should not previously exist
  assert(Ctx->findNode(X) == nullptr && "UUID already registered");

  Ctx->unregisterNode(this);
  this->Uuid = X;
  Ctx->registerNode(Uuid, this);
}

std::string gtirb::uuidToString(const UUID& Uuid) {
  return boost::uuids::to_string(Uuid);
}

UUID gtirb::uuidFromString(const std::string& Text) {
  return boost::uuids::string_generator()(Text);
}

void GTIRB_EXPORT_API details::ClearUUIDs(Node *N) {
  N->setUUID();

  if (auto *I = dyn_cast<IR>(N)) {
    for (auto &M : I->modules()) {
      details::ClearUUIDs(&M);
    }
  } else if (auto *M = dyn_cast<Module>(N)) {
    details::ClearUUIDs(&M->getImageByteMap());
    for (auto &D : M->data()) {
      details::ClearUUIDs(&D);
    }
    for (auto &S : M->sections()) {
      details::ClearUUIDs(&S);
    }
    for (auto &S : M->symbols()) {
      details::ClearUUIDs(&S);
    }
    for (auto &B : blocks(M->getCFG())) {
      details::ClearUUIDs(&B);
    }
  }
}
