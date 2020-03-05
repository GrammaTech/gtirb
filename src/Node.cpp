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
#include "gtirb/DataBlock.hpp"
#include "gtirb/IR.hpp"
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

template <typename NodeType, typename CollectionType>
static void modifyIndex(CollectionType& Index, NodeType* N,
                        const std::function<void()>& F) {
  if (auto It = Index.find(N); It != Index.end()) {
    Index.modify(It, [&F](const auto&) { F(); });
  } else {
    F();
  }
}

static uint64_t extractSize(uint64_t t) { return t; }
static uint64_t extractSize(std::optional<uint64_t> t) { return *t; }

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
  case Node::Kind::ByteInterval: {
    auto* BI = cast<ByteInterval>(this);
    auto* S = BI->getSection();
    if (!S) {
      return;
    }
    addToICL(S->ByteIntervalAddrs, BI);
    // Updating the symbol index isn't necesary, because no symbols are defined
    // pointing to blocks in this interval if you're adding a whole new one,
    // and the address will not change if you're moving an interval from
    // one section to another.
  } break;
  case Node::Kind::CodeBlock: {
    auto* B = cast<CodeBlock>(this);

    auto* BI = B->getByteInterval();
    if (!BI) {
      return;
    }

    addToICL(BI->BlockAddrs, &BI->nodeToBlock(B), B->getAddress(),
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
    for (auto& Sym : M->findSymbols(*B)) {
      modifyIndex(M->Symbols.get<Module::by_pointer>(), &Sym, []() {});
    }

    // Update CFG.
    if (IR* P = M->getIR()) {
      addVertex(B, P->getCFG());
    }
  } break;
  case Node::Kind::DataBlock: {
    auto* B = cast<DataBlock>(this);

    auto* BI = B->getByteInterval();
    if (!BI) {
      return;
    }

    addToICL(BI->BlockAddrs, &BI->nodeToBlock(B), B->getAddress(),
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
    for (auto& Sym : M->findSymbols(*B)) {
      modifyIndex(M->Symbols.get<Module::by_pointer>(), &Sym, []() {});
    }
  } break;
  case Node::Kind::Section: {
    auto* S = cast<Section>(this);
    auto* M = S->getModule();
    if (!M) {
      return;
    }
    addToICL(M->SectionAddrs, S);
  } break;
  default: { assert(!"unexpected kind of node passed to addToModuleIndices!"); }
  }
}

void Node::mutateIndices(const std::function<void()>& F) {
  switch (getKind()) {
  case Node::Kind::ByteInterval: {
    auto* BI = cast<ByteInterval>(this);
    auto* S = BI->getSection();
    if (!S) {
      F();
      return;
    }
    removeFromICL(S->ByteIntervalAddrs, BI);
    S->mutateIndices([&]() {
      modifyIndex(S->ByteIntervals.get<Section::by_pointer>(), BI, F);
    });
    addToICL(S->ByteIntervalAddrs, BI);

    // Symbols may need their address index updated if they refer to a block
    // inside this BI.
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
    removeFromICL(BI->BlockAddrs, Blk, B->getAddress(), B->getSize());
    BI->mutateIndices([&]() {
      modifyIndex(BI->Blocks.get<ByteInterval::by_pointer>(), B, F);
    });
    addToICL(BI->BlockAddrs, Blk, B->getAddress(), B->getSize());
  } break;
  case Node::Kind::DataBlock: {
    auto* B = cast<DataBlock>(this);
    auto* BI = B->getByteInterval();
    if (!BI) {
      F();
      return;
    }
    auto* Blk = &BI->nodeToBlock(B);
    removeFromICL(BI->BlockAddrs, Blk, B->getAddress(), B->getSize());
    BI->mutateIndices([&]() {
      modifyIndex(BI->Blocks.get<ByteInterval::by_pointer>(), B, F);
    });
    addToICL(BI->BlockAddrs, Blk, B->getAddress(), B->getSize());
  } break;
  case Node::Kind::Section: {
    auto* S = cast<Section>(this);
    auto* M = S->getModule();
    if (!M) {
      F();
      return;
    }
    removeFromICL(M->SectionAddrs, S);
    M->mutateIndices(
        [&]() { modifyIndex(M->Sections.get<Module::by_pointer>(), S, F); });
    addToICL(M->SectionAddrs, S);
  } break;
  case Node::Kind::Symbol: {
    auto* S = cast<Symbol>(this);
    auto* M = S->getModule();
    if (!M) {
      F();
      return;
    }
    M->mutateIndices(
        [&]() { modifyIndex(M->Symbols.get<Module::by_pointer>(), S, F); });
  } break;
  case Node::Kind::Module: {
    auto* M = cast<Module>(this);
    auto* I = M->getIR();
    if (!I) {
      F();
      return;
    }
    modifyIndex(I->Modules.get<IR::by_pointer>(), M, F);
  } break;
  default: {
    assert(!"unexpected kind of node passed to mutateModuleIndices!");
  }
  }
}

void Node::removeFromIndices() {
  switch (getKind()) {
  case Node::Kind::ByteInterval: {
    auto* BI = cast<ByteInterval>(this);
    auto* S = BI->getSection();
    if (!S) {
      return;
    }
    removeFromICL(S->ByteIntervalAddrs, BI);
    // Updating the symbol index isn't necesary, because no symbols should be
    // defined pointing to blocks in this interval if you're removing one,
    // and the address will not change if you're moving an interval from
    // one section to another.
  } break;
  case Node::Kind::CodeBlock: {
    auto* B = cast<CodeBlock>(this);

    auto* BI = B->getByteInterval();
    if (!BI) {
      return;
    }

    removeFromICL(BI->BlockAddrs, &BI->nodeToBlock(B), B->getAddress(),
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
    for (auto& Sym : M->findSymbols(*B)) {
      modifyIndex(M->Symbols.get<Module::by_pointer>(), &Sym, []() {});
    }

    // Update CFG.
    if (IR* P = M->getIR()) {
      removeVertex(B, P->getCFG());
    }
  } break;
  case Node::Kind::DataBlock: {
    auto* B = cast<DataBlock>(this);

    auto* BI = B->getByteInterval();
    if (!BI) {
      return;
    }

    removeFromICL(BI->BlockAddrs, &BI->nodeToBlock(B), B->getAddress(),
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
    for (auto& Sym : M->findSymbols(*B)) {
      modifyIndex(M->Symbols.get<Module::by_pointer>(), &Sym, []() {});
    }
  } break;
  case Node::Kind::Section: {
    auto* S = cast<Section>(this);
    auto* M = S->getModule();
    if (!M) {
      return;
    }
    removeFromICL(M->SectionAddrs, S);
  } break;
  default: {
    assert(!"unexpected kind of node passed to mutateModuleIndices!");
  }
  }
}
