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
#include "gtirb/CodeBlock.hpp"
#include "gtirb/DataBlock.hpp"
#include "gtirb/IR.hpp"
#include "gtirb/Module.hpp"
#include "gtirb/Section.hpp"
#include "gtirb/SymbolicExpression.hpp"
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

static uint64_t extractSize(std::optional<uint64_t> t) { return *t; }

template <typename NodeType, typename IntMapType, typename SizeType>
static void addToICL(IntMapType& IntMap, NodeType* N, uint64_t Off,
                     SizeType S) {
  IntMap.add(std::make_pair(
      IntMapType::interval_type::right_open(Off, Off + extractSize(S)),
      typename IntMapType::codomain_type({N})));
}

template <typename NodeType, typename IntMapType, typename SizeType>
static void addToICL(IntMapType& IntMap, NodeType* N, std::optional<Addr> A,
                     SizeType S) {
  if (A) {
    IntMap.add(std::make_pair(
        IntMapType::interval_type::right_open(*A, *A + extractSize(S)),
        typename IntMapType::codomain_type({N})));
  }
}

template <typename NodeType, typename IntMapType, typename SizeType>
static void removeFromICL(IntMapType& IntMap, NodeType* N, uint64_t Off,
                          SizeType S) {
  IntMap.subtract(std::make_pair(
      IntMapType::interval_type::right_open(Off, Off + extractSize(S)),
      typename IntMapType::codomain_type({N})));
}

template <typename NodeType, typename IntMapType, typename SizeType>
static void removeFromICL(IntMapType& IntMap, NodeType* N,
                          std::optional<Addr> A, SizeType S) {
  if (A) {
    IntMap.subtract(std::make_pair(
        IntMapType::interval_type::right_open(*A, *A + extractSize(S)),
        typename IntMapType::codomain_type({N})));
  }
}

template <typename NodeType, typename IntMapType>
static void addToICL(IntMapType& IntMap, NodeType* N) {
  return addToICL(IntMap, N, N->getAddress(), N->getSize());
}

template <typename NodeType, typename IntMapType>
static void removeFromICL(IntMapType& IntMap, NodeType* N) {
  return removeFromICL(IntMap, N, N->getAddress(), N->getSize());
}

// FIXME: For the internals of these index functions, it'd be nice to have
// some sort of virtual dispatch nstead of one big switch. But how to do that
// without (a) adding in RTTI or (b) making everything friends of each other...

// FIXME: It would also be nice to be more discerning about what indices to
// update, so we invalidate only the minimum of iterators. Right now, modifying
// many properties invalidates more iterators that it strictly needs to.

void Node::addToIndices() {
  switch (getKind()) {
  case Node::Kind::CodeBlock: {
    auto* B = cast<CodeBlock>(this);

    auto* BI = B->getByteInterval();
    if (!BI) {
      return;
    }

    addToICL(BI->BlockOffsets, &BI->nodeToBlock(B), B->getOffset(),
             B->getSize());

    auto* S = BI->getSection();
    if (!S) {
      return;
    }

    auto* M = S->getModule();
    if (!M) {
      return;
    }

    // Update symbol referents.
    // Note that we update the symbol's address index while iterating over its
    // referent index, so one doesn't invalidate the other.
    for (auto& Sym : M->findSymbols(*B)) {
      modifyIndex(M->Symbols.get<Module::by_pointer>(), &Sym, []() {});
    }

    // Update CFG.
    if (IR* P = M->getIR()) {
      addVertex(B, P->Cfg);
    }
  } break;
  case Node::Kind::DataBlock: {
    auto* B = cast<DataBlock>(this);

    auto* BI = B->getByteInterval();
    if (!BI) {
      return;
    }

    addToICL(BI->BlockOffsets, &BI->nodeToBlock(B), B->getOffset(),
             B->getSize());

    auto* S = BI->getSection();
    if (!S) {
      return;
    }

    auto* M = S->getModule();
    if (!M) {
      return;
    }

    // Update symbol referents.
    // Note that we update the symbol's address index while iterating over its
    // referent index, so one doesn't invalidate the other.
    for (auto& Sym : M->findSymbols(*B)) {
      modifyIndex(M->Symbols.get<Module::by_pointer>(), &Sym, []() {});
    }
  } break;
  default: { assert(!"unexpected kind of node passed to addToModuleIndices!"); }
  }
}

void Node::mutateIndices(const std::function<void()>& F) {
  switch (getKind()) {
  case Node::Kind::ByteInterval: {
    F();

    auto* BI = cast<ByteInterval>(this);
    auto* S = BI->getSection();
    if (!S) {
      return;
    }

    // Symbols may need their address index updated if they refer to a block
    // inside this BI.
    // Note that we update the symbol's address index while iterating over its
    // referent index, so one doesn't invalidate the other.
    auto* M = S->getModule();
    if (!M) {
      return;
    }

    for (auto& B : BI->blocks()) {
      for (auto& Sym : M->findSymbols(B)) {
        modifyIndex(M->Symbols.get<Module::by_pointer>(), &Sym, []() {});
      }
    }
  } break;
  case Node::Kind::CodeBlock: {
    auto* B = cast<CodeBlock>(this);
    auto* BI = B->getByteInterval();
    if (!BI) {
      F();
      return;
    }
    auto* Blk = &BI->nodeToBlock(B);
    removeFromICL(BI->BlockOffsets, Blk, B->getOffset(), B->getSize());
    modifyIndex(BI->Blocks.get<ByteInterval::by_pointer>(), B, F);
    addToICL(BI->BlockOffsets, Blk, B->getOffset(), B->getSize());
  } break;
  case Node::Kind::DataBlock: {
    auto* B = cast<DataBlock>(this);
    auto* BI = B->getByteInterval();
    if (!BI) {
      F();
      return;
    }
    auto* Blk = &BI->nodeToBlock(B);
    removeFromICL(BI->BlockOffsets, Blk, B->getOffset(), B->getSize());
    modifyIndex(BI->Blocks.get<ByteInterval::by_pointer>(), B, F);
    addToICL(BI->BlockOffsets, Blk, B->getOffset(), B->getSize());
  } break;
  case Node::Kind::Section: {
    auto* S = cast<Section>(this);
    std::optional<AddrRange> OldExtent = addressRange(*S);
    F();
    if (S->Observer) {
      [[maybe_unused]] ChangeStatus status =
          S->Observer->changeExtent(S, OldExtent, addressRange(*S));
      assert(status != ChangeStatus::REJECTED &&
             "recovering from rejected removal is not implemented yet");
    }
  } break;
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
  switch (getKind()) {
  case Node::Kind::CodeBlock: {
    auto* B = cast<CodeBlock>(this);

    auto* BI = B->getByteInterval();
    if (!BI) {
      return;
    }

    removeFromICL(BI->BlockOffsets, &BI->nodeToBlock(B), B->getOffset(),
                  B->getSize());

    auto* S = BI->getSection();
    if (!S) {
      return;
    }

    auto* M = S->getModule();
    if (!M) {
      return;
    }

    // Update symbol referents.
    // Note that we update the symbol's address index while iterating over its
    // referent index, so one doesn't invalidate the other.
    for (auto& Sym : M->findSymbols(*B)) {
      modifyIndex(M->Symbols.get<Module::by_pointer>(), &Sym, []() {});
    }

    // Update CFG.
    if (IR* P = M->getIR()) {
      removeVertex(B, P->Cfg);
    }
  } break;
  case Node::Kind::DataBlock: {
    auto* B = cast<DataBlock>(this);

    auto* BI = B->getByteInterval();
    if (!BI) {
      return;
    }

    removeFromICL(BI->BlockOffsets, &BI->nodeToBlock(B), B->getOffset(),
                  B->getSize());

    auto* S = BI->getSection();
    if (!S) {
      return;
    }

    auto* M = S->getModule();
    if (!M) {
      return;
    }

    // Update symbol referents.
    // Note that we update the symbol's address index while iterating over its
    // referent index, so one doesn't invalidate the other.
    for (auto& Sym : M->findSymbols(*B)) {
      modifyIndex(M->Symbols.get<Module::by_pointer>(), &Sym, []() {});
    }
  } break;
  default: {
    assert(!"unexpected kind of node passed to mutateModuleIndices!");
  }
  }
}
