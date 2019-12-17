//===- Module.cpp -----------------------------------------------*- C++ -*-===//
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
#include "Module.hpp"
#include <gtirb/CFG.hpp>
#include <gtirb/CodeBlock.hpp>
#include <gtirb/IR.hpp>
#include <gtirb/Serialization.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <proto/Module.pb.h>
#include <map>

using namespace gtirb;

void Module::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_binary_path(this->BinaryPath);
  Message->set_preferred_addr(static_cast<uint64_t>(this->PreferredAddr));
  Message->set_rebase_delta(this->RebaseDelta);
  Message->set_file_format(static_cast<proto::FileFormat>(this->FileFormat));
  Message->set_isa(static_cast<proto::ISA>(this->Isa));
  Message->set_name(this->Name);
  sequenceToProtobuf(ProxyBlocks.begin(), ProxyBlocks.end(),
                     Message->mutable_proxies());
  sequenceToProtobuf(sections_begin(), sections_end(),
                     Message->mutable_sections());
  containerToProtobuf(Symbols, Message->mutable_symbols());
  if (EntryPoint) {
    nodeUUIDToBytes(EntryPoint, *Message->mutable_entry_point());
  }
  AuxDataContainer::toProtobuf(Message);
}

// FIXME: improve containerFromProtobuf so it can handle a pair where one
// element is a pointer to a Node subclass.
template <class T, class U, class V, class W>
static void nodeMapFromProtobuf(Context& C, std::map<T, U*>& Values,
                                const google::protobuf::Map<V, W>& Message) {
  Values.clear();
  std::for_each(Message.begin(), Message.end(), [&Values, &C](const auto& M) {
    std::pair<T, U*> Val;
    fromProtobuf(C, Val.first, M.first);
    Val.second = U::fromProtobuf(C, M.second);
    Values.insert(std::move(Val));
  });
}

Module* Module::fromProtobuf(Context& C, IR* Parent,
                             const MessageType& Message) {
  Module* M = Module::Create(C);
  M->setIR(Parent);
  setNodeUUIDFromBytes(M, Message.uuid());
  M->BinaryPath = Message.binary_path();
  M->PreferredAddr = Addr(Message.preferred_addr());
  M->RebaseDelta = Message.rebase_delta();
  M->FileFormat = static_cast<gtirb::FileFormat>(Message.file_format());
  M->Isa = static_cast<ISA>(Message.isa());
  M->Name = Message.name();
  for (const auto& Elt : Message.proxies()) {
    auto* B = ProxyBlock::fromProtobuf(C, M, Elt);
    M->ProxyBlocks.insert(B);
    B->setModule(M);
    addVertex(B, M->Cfg);
  }
  for (const auto& Elt : Message.sections()) {
    auto* S = Section::fromProtobuf(C, M, Elt);
    M->Sections.emplace(S);
    S->setModule(M);
    S->addToIndices();
  }
  for (const auto& Elt : Message.symbols()) {
    auto* S = Symbol::fromProtobuf(C, M, Elt);
    M->Symbols.emplace(S);
    S->setModule(M);
  }
  for (const auto& ProtoS : Message.sections()) {
    for (const auto& ProtoBI : ProtoS.byte_intervals()) {
      auto* BI =
          cast<ByteInterval>(getByUUID(C, uuidFromBytes(ProtoBI.uuid())));
      BI->symbolicExpressionsFromProtobuf(C, ProtoBI);
    }
  }
  if (!Message.entry_point().empty()) {
    M->EntryPoint = cast<CodeBlock>(
        Node::getByUUID(C, uuidFromBytes(Message.entry_point())));
  }
  static_cast<AuxDataContainer*>(M)->fromProtobuf(C, Message);
  return M;
}

void Module::moveProxyBlock(ProxyBlock* B) {
  if (B->getModule()) {
    B->getModule()->removeProxyBlock(B);
  }
  ProxyBlocks.insert(B);
  B->setModule(this);
  if (Parent)
    addVertex(B, Parent->getCFG());
}

void Module::removeProxyBlock(ProxyBlock* B) {
  ProxyBlocks.erase(B);
  B->setModule(nullptr);
  if (Parent)
    removeVertex(B, Parent->getCFG());
}

void Module::addProxyBlock(ProxyBlock* B) {
  ProxyBlocks.insert(B);
  if (Parent)
    addVertex(B, Parent->getCFG());
}

template <typename NodeType, typename CollectionType>
static void modifyIndex(CollectionType& Index, NodeType* N,
                        const std::function<void()>& F) {
  if (auto it = Index.find(N); it != Index.end()) {
    Index.modify(it, [&F](const auto&) { F(); });
  } else {
    F();
  }
}

static uint64_t extractSize(uint64_t t) { return t; }
static uint64_t extractSize(std::optional<uint64_t> t) { return *t; }

template <typename NodeType, typename IntMapType>
static void addToICL(IntMapType& IntMap, NodeType* N) {
  auto addr = N->getAddress();
  if (addr) {
    IntMap.add(std::make_pair(IntMapType::interval_type::right_open(
                                  *addr, *addr + extractSize(N->getSize())),
                              typename IntMapType::codomain_type({N})));
  }
}

template <typename NodeType, typename IntMapType>
static void removeFromICL(IntMapType& IntMap, NodeType* N) {
  auto addr = N->getAddress();
  if (addr) {
    IntMap.subtract(
        std::make_pair(IntMapType::interval_type::right_open(
                           *addr, *addr + extractSize(N->getSize())),
                       typename IntMapType::codomain_type({N})));
  }
}

void gtirb::addToModuleIndices(Node* N) {
  switch (N->getKind()) {
  case Node::Kind::ByteInterval: {
    auto BI = cast<ByteInterval>(N);
    auto S = BI ? BI->getSection() : nullptr;
    auto M = S ? S->getModule() : nullptr;
    if (M) {
      M->ByteIntervals.insert(BI);

      addToICL(M->ByteIntervalAddrs, BI);

      for (auto& B : BI->blocks()) {
        addToModuleIndices(B.getNode());
      }
      for (auto& SE : BI->symbolic_expressions()) {
        M->SymbolicExpressions.emplace(BI, SE.first);
      }
    }
  } break;
  case Node::Kind::CodeBlock: {
    auto B = cast<CodeBlock>(N);
    auto BI = B->getByteInterval();
    auto S = BI ? BI->getSection() : nullptr;
    auto M = S ? S->getModule() : nullptr;
    if (M) {
      M->CodeBlocks.insert(B);
      addToICL(M->CodeBlockAddrs, B);
      if (auto* I = M->getIR())
        addVertex(B, I->getCFG());
    }
  } break;
  case Node::Kind::DataBlock: {
    auto B = cast<DataBlock>(N);
    auto BI = B->getByteInterval();
    auto S = BI ? BI->getSection() : nullptr;
    auto M = S ? S->getModule() : nullptr;
    if (M) {
      M->DataBlocks.insert(B);
      addToICL(M->DataBlockAddrs, B);
    }
  } break;
  case Node::Kind::Section: {
    auto S = cast<Section>(N);
    auto M = S->getModule();
    if (M) {
      M->Sections.insert(S);
      for (auto BI : S->byte_intervals()) {
        addToModuleIndices(BI);
        addToICL(M->SectionAddrs, S);
      }
    }
  } break;
  case Node::Kind::Symbol: {
    auto S = cast<Symbol>(N);
    auto M = S->getModule();
    if (M) {
      M->Symbols.insert(S);
    }
  } break;
  default: { assert(!"unexpected kind of node passed to addToModuleIndices!"); }
  }
}

void gtirb::mutateModuleIndices(Node* N, const std::function<void()>& F) {
  switch (N->getKind()) {
  case Node::Kind::ByteInterval: {
    auto BI = cast<ByteInterval>(N);
    auto S = BI ? BI->getSection() : nullptr;
    auto M = S ? S->getModule() : nullptr;
    if (M) {
      removeFromICL(M->SectionAddrs, S);
      removeFromICL(M->ByteIntervalAddrs, BI);

      auto& seIndex = M->SymbolicExpressions.get<Module::by_pointer>();
      for (auto& SE : BI->symbolic_expressions()) {
        if (auto it = seIndex.find(std::make_pair(BI, SE.first));
            it != seIndex.end()) {
          seIndex.erase(it);
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
    auto B = cast<CodeBlock>(N);
    auto BI = B->getByteInterval();
    auto S = BI ? BI->getSection() : nullptr;
    auto M = S ? S->getModule() : nullptr;
    if (M) {
      removeFromICL(M->CodeBlockAddrs, B);
      modifyIndex(M->CodeBlocks.get<Module::by_pointer>(), B, F);
      addToICL(M->CodeBlockAddrs, B);
    } else {
      F();
    }
  } break;
  case Node::Kind::DataBlock: {
    auto B = cast<DataBlock>(N);
    auto BI = B->getByteInterval();
    auto S = BI ? BI->getSection() : nullptr;
    auto M = S ? S->getModule() : nullptr;
    if (M) {
      removeFromICL(M->DataBlockAddrs, B);
      modifyIndex(M->DataBlocks.get<Module::by_pointer>(), B, F);
      addToICL(M->DataBlockAddrs, B);
    } else {
      F();
    }
  } break;
  case Node::Kind::Section: {
    auto S = cast<Section>(N);
    auto M = S->getModule();
    if (M) {
      removeFromICL(M->SectionAddrs, S);
      modifyIndex(M->Sections.get<Module::by_pointer>(), S, F);
      addToICL(M->SectionAddrs, S);
    } else {
      F();
    }
  } break;
  case Node::Kind::Symbol: {
    auto S = cast<Symbol>(N);
    auto M = S->getModule();
    if (M) {
      modifyIndex(M->Symbols.get<Module::by_pointer>(), S, F);
    } else {
      F();
    }
  } break;
  default: {
    assert(!"unexpected kind of node passed to mutateModuleIndices!");
  }
  }
}

void gtirb::removeFromModuleIndices(Node* N) {
  switch (N->getKind()) {
  case Node::Kind::ByteInterval: {
    auto BI = cast<ByteInterval>(N);
    auto S = BI ? BI->getSection() : nullptr;
    auto M = S ? S->getModule() : nullptr;
    if (M) {
      removeFromICL(M->ByteIntervalAddrs, BI);

      auto& index = M->ByteIntervals.get<Module::by_pointer>();
      if (auto it = index.find(BI); it != index.end()) {
        index.erase(it);
      }

      for (auto& B : BI->blocks()) {
        removeFromModuleIndices(B.getNode());
      }

      auto& seIndex = M->SymbolicExpressions.get<Module::by_pointer>();
      for (auto& SE : BI->symbolic_expressions()) {
        if (auto it = seIndex.find(std::make_pair(BI, SE.first));
            it != seIndex.end()) {
          seIndex.erase(it);
        }
      }
    }
  } break;
  case Node::Kind::CodeBlock: {
    auto B = cast<CodeBlock>(N);
    auto BI = B->getByteInterval();
    auto S = BI ? BI->getSection() : nullptr;
    auto M = S ? S->getModule() : nullptr;
    if (M) {
      removeFromICL(M->CodeBlockAddrs, B);
      auto& index = M->CodeBlocks.get<Module::by_pointer>();
      if (auto it = index.find(B); it != index.end()) {
        index.erase(it);
      }
      if (auto* Ir = M->getIR())
        removeVertex(B, Ir->getCFG());
    }
  } break;
  case Node::Kind::DataBlock: {
    auto B = cast<DataBlock>(N);
    auto BI = B->getByteInterval();
    auto S = BI ? BI->getSection() : nullptr;
    auto M = S ? S->getModule() : nullptr;
    if (M) {
      removeFromICL(M->DataBlockAddrs, B);
      auto& index = M->DataBlocks.get<Module::by_pointer>();
      if (auto it = index.find(B); it != index.end()) {
        index.erase(it);
      }
    }
  } break;
  case Node::Kind::Section: {
    auto S = cast<Section>(N);
    auto M = S->getModule();
    if (M) {
      removeFromICL(M->SectionAddrs, S);
      auto& index = M->Sections.get<Module::by_pointer>();
      if (auto it = index.find(S); it != index.end()) {
        index.erase(it);
      }
      for (auto BI : S->byte_intervals()) {
        removeFromModuleIndices(BI);
      }
    }
  } break;
  case Node::Kind::Symbol: {
    auto S = cast<Symbol>(N);
    auto M = S->getModule();
    if (M) {
      auto& index = M->Symbols.get<Module::by_pointer>();
      if (auto it = index.find(S); it != index.end()) {
        index.erase(it);
      }
    }
  } break;
  default: {
    assert(!"unexpected kind of node passed to mutateModuleIndices!");
  }
  }
}
