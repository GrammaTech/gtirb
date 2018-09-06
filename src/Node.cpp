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

