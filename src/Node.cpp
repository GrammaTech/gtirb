//===- Node.cpp -------------------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2018-2019 GrammaTech, Inc.
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

template <typename NodeType, typename IntMapType>
static void addToICL(IntMapType& IntMap, NodeType* N) {
  if (auto A = N->getAddress()) {
    IntMap.add(std::make_pair(IntMapType::interval_type::right_open(
                                  *A, *A + extractSize(N->getSize())),
                              typename IntMapType::codomain_type({N})));
  }
}

template <typename NodeType, typename IntMapType>
static void removeFromICL(IntMapType& IntMap, NodeType* N) {
  if (auto A = N->getAddress()) {
    IntMap.subtract(std::make_pair(IntMapType::interval_type::right_open(
                                       *A, *A + extractSize(N->getSize())),
                                   typename IntMapType::codomain_type({N})));
  }
}

void Node::addToIndices() {
  switch (getKind()) {
  case Node::Kind::ByteInterval: {
    auto* BI = cast<ByteInterval>(this);
    auto* S = BI->getSection();
    auto* M = S ? S->getModule() : nullptr;
    if (M) {
      M->ByteIntervals.insert(BI);

      addToICL(M->ByteIntervalAddrs, BI);

      for (auto& B : BI->blocks()) {
        B.addToIndices();
      }
      for (auto& SE : BI->symbolic_expressions()) {
        M->SymbolicExpressions.emplace(BI, SE.first);
      }
    }
  } break;
  case Node::Kind::CodeBlock: {
    auto* B = cast<CodeBlock>(this);
    auto* BI = B->getByteInterval();
    auto* S = BI ? BI->getSection() : nullptr;
    auto* M = S ? S->getModule() : nullptr;
    if (M) {
      M->CodeBlocks.insert(B);
      addToICL(M->CodeBlockAddrs, B);
      addVertex(B, M->getCFG());
    }
  } break;
  case Node::Kind::DataBlock: {
    auto* B = cast<DataBlock>(this);
    auto* BI = B->getByteInterval();
    auto* S = BI ? BI->getSection() : nullptr;
    auto* M = S ? S->getModule() : nullptr;
    if (M) {
      M->DataBlocks.insert(B);
      addToICL(M->DataBlockAddrs, B);
    }
  } break;
  case Node::Kind::Section: {
    auto* S = cast<Section>(this);
    auto* M = S->getModule();
    if (M) {
      M->Sections.insert(S);
      for (auto* BI : S->byte_intervals()) {
        BI->addToIndices();
        addToICL(M->SectionAddrs, S);
      }
    }
  } break;
  case Node::Kind::Symbol: {
    auto* S = cast<Symbol>(this);
    auto* M = S->getModule();
    if (M) {
      M->Symbols.insert(S);
    }
  } break;
  case Node::Kind::Module: {
    auto* M = cast<Module>(this);
    auto* I = M->getIR();
    if (I) {
      I->Modules.insert(M);
    }
  } break;
  default: { assert(!"unexpected kind of node passed to addToModuleIndices!"); }
  }
}

void Node::mutateIndices(const std::function<void()>& F) {
  switch (getKind()) {
  case Node::Kind::ByteInterval: {
    auto* BI = cast<ByteInterval>(this);
    auto* S = BI->getSection();
    auto* M = S ? S->getModule() : nullptr;
    if (M) {
      removeFromICL(M->SectionAddrs, S);
      removeFromICL(M->ByteIntervalAddrs, BI);

      auto& SEIndex = M->SymbolicExpressions.get<Module::by_pointer>();
      for (auto& SE : BI->symbolic_expressions()) {
        if (auto It = SEIndex.find(std::make_pair(BI, SE.first));
            It != SEIndex.end()) {
          SEIndex.erase(It);
        }
      }

      modifyIndex(M->Sections.get<Module::by_pointer>(), S, [&]() {
        modifyIndex(M->ByteIntervals.get<Module::by_pointer>(), BI, F);
      });

      for (auto& SE : BI->symbolic_expressions()) {
        M->SymbolicExpressions.emplace(BI, SE.first);
      }

      addToICL(M->ByteIntervalAddrs, BI);
      addToICL(M->SectionAddrs, S);
    } else {
      F();
    }
  } break;
  case Node::Kind::CodeBlock: {
    auto* B = cast<CodeBlock>(this);
    auto* BI = B->getByteInterval();
    auto* S = BI ? BI->getSection() : nullptr;
    auto* M = S ? S->getModule() : nullptr;
    if (M) {
      removeFromICL(M->CodeBlockAddrs, B);
      modifyIndex(M->CodeBlocks.get<Module::by_pointer>(), B, F);
      addToICL(M->CodeBlockAddrs, B);
    } else {
      F();
    }
  } break;
  case Node::Kind::DataBlock: {
    auto* B = cast<DataBlock>(this);
    auto* BI = B->getByteInterval();
    auto* S = BI ? BI->getSection() : nullptr;
    auto* M = S ? S->getModule() : nullptr;
    if (M) {
      removeFromICL(M->DataBlockAddrs, B);
      modifyIndex(M->DataBlocks.get<Module::by_pointer>(), B, F);
      addToICL(M->DataBlockAddrs, B);
    } else {
      F();
    }
  } break;
  case Node::Kind::Section: {
    auto* S = cast<Section>(this);
    auto* M = S->getModule();
    if (M) {
      removeFromICL(M->SectionAddrs, S);
      modifyIndex(M->Sections.get<Module::by_pointer>(), S, F);
      addToICL(M->SectionAddrs, S);
    } else {
      F();
    }
  } break;
  case Node::Kind::Symbol: {
    auto* S = cast<Symbol>(this);
    auto* M = S->getModule();
    if (M) {
      modifyIndex(M->Symbols.get<Module::by_pointer>(), S, F);
    } else {
      F();
    }
  } break;
  case Node::Kind::Module: {
    auto* M = cast<Module>(this);
    auto* I = M->getIR();
    if (I) {
      modifyIndex(I->Modules.get<IR::by_pointer>(), M, F);
    } else {
      F();
    }
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
    auto* M = S ? S->getModule() : nullptr;
    if (M) {
      removeFromICL(M->ByteIntervalAddrs, BI);

      auto& Index = M->ByteIntervals.get<Module::by_pointer>();
      if (auto It = Index.find(BI); It != Index.end()) {
        Index.erase(It);
      }

      for (auto& B : BI->blocks()) {
        B.removeFromIndices();
      }

      auto& SEIndex = M->SymbolicExpressions.get<Module::by_pointer>();
      for (auto& SE : BI->symbolic_expressions()) {
        if (auto It = SEIndex.find(std::make_pair(BI, SE.first));
            It != SEIndex.end()) {
          SEIndex.erase(It);
        }
      }
    }
  } break;
  case Node::Kind::CodeBlock: {
    auto* B = cast<CodeBlock>(this);
    auto* BI = B->getByteInterval();
    auto* S = BI ? BI->getSection() : nullptr;
    auto* M = S ? S->getModule() : nullptr;
    if (M) {
      removeFromICL(M->CodeBlockAddrs, B);
      auto& Index = M->CodeBlocks.get<Module::by_pointer>();
      if (auto It = Index.find(B); It != Index.end()) {
        Index.erase(It);
      }
      // removeVertex(B, M->getCFG());
    }
  } break;
  case Node::Kind::DataBlock: {
    auto* B = cast<DataBlock>(this);
    auto* BI = B->getByteInterval();
    auto* S = BI ? BI->getSection() : nullptr;
    auto* M = S ? S->getModule() : nullptr;
    if (M) {
      removeFromICL(M->DataBlockAddrs, B);
      auto& Index = M->DataBlocks.get<Module::by_pointer>();
      if (auto It = Index.find(B); It != Index.end()) {
        Index.erase(It);
      }
    }
  } break;
  case Node::Kind::Section: {
    auto* S = cast<Section>(this);
    auto* M = S->getModule();
    if (M) {
      removeFromICL(M->SectionAddrs, S);
      auto& Index = M->Sections.get<Module::by_pointer>();
      if (auto It = Index.find(S); It != Index.end()) {
        Index.erase(It);
      }
      for (auto BI : S->byte_intervals()) {
        BI->removeFromIndices();
      }
    }
  } break;
  case Node::Kind::Symbol: {
    auto* S = cast<Symbol>(this);
    auto* M = S->getModule();
    if (M) {
      auto& Index = M->Symbols.get<Module::by_pointer>();
      if (auto It = Index.find(S); It != Index.end()) {
        Index.erase(It);
      }
    }
  } break;
  case Node::Kind::Module: {
    auto* M = cast<Module>(this);
    auto* I = M->getIR();
    if (I) {
      auto& Index = I->Modules.get<IR::by_pointer>();
      if (auto It = Index.find(M); It != Index.end()) {
        Index.erase(It);
      }
    }
  } break;
  default: {
    assert(!"unexpected kind of node passed to mutateModuleIndices!");
  }
  }
}
