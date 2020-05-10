//===- Node.cpp -------------------------------------------------*- C++ -*-===//
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
#include "Node.hpp"
#include "gtirb/Module.hpp"
#include <boost/uuid/uuid_generators.hpp>

using namespace gtirb;

// TODO: accessing this object between threads requires synchronization.
static boost::uuids::random_generator UUIDGenerator;

Node::Node(Context& C, Kind Knd) : K(Knd), Uuid(UUIDGenerator()), Ctx(&C) {
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

template <typename NodeType, typename CollectionType>
static void modifyIndex(CollectionType& Index, NodeType* N,
                        const std::function<void()>& F) {
  if (auto It = Index.find(N); It != Index.end()) {
    Index.modify(It, [&F](const auto&) { F(); });
  } else {
    F();
  }
}

// FIXME: It would also be nice to be more discerning about what indices to
// update, so we invalidate only the minimum of iterators. Right now, modifying
// many properties invalidates more iterators that it strictly needs to.

void Node::addToIndices() {
  assert(!"unexpected kind of node passed to addToModuleIndices!");
}

void Node::mutateIndices(const std::function<void()>& F) {
  switch (getKind()) {
  case Node::Kind::Symbol: {
    auto* S = cast<Symbol>(this);
    auto* M = S->getModule();
    if (!M) {
      F();
      return;
    }
    modifyIndex(M->Symbols.get<Module::by_pointer>(), S, F);
  } break;
  default: {
    assert(!"unexpected kind of node passed to mutateModuleIndices!");
  }
  }
}

void Node::removeFromIndices() {
  assert(!"unexpected kind of node passed to mutateModuleIndices!");
}
