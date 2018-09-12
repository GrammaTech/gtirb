#include "Node.hpp"
#include "gtirb/DataObject.hpp"
#include "gtirb/IR.hpp"
#include "gtirb/ImageByteMap.hpp"
#include "gtirb/Module.hpp"
#include "gtirb/Section.hpp"
#include "gtirb/SymbolicExpression.hpp"
#include <boost/uuid/uuid_generators.hpp>

using namespace gtirb;

Node::Node(Context& C, Kind Knd)
    : K(Knd), Uuid(boost::uuids::random_generator()()), Ctx(&C) {
  Ctx->registerNode(Uuid, this);
}

Node::~Node() noexcept { Ctx->unregisterNode(this); }

void Node::setUUID(UUID X) {
  // UUID should not previously exist
  assert(Ctx->findNode(X) == nullptr && "UUID already registered");

  Ctx->unregisterNode(this);
  this->Uuid = X;
  Ctx->registerNode(Uuid, this);
}
