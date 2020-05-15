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
#include "Serialization.hpp"
#include <gtirb/CFG.hpp>
#include <gtirb/CodeBlock.hpp>
#include <gtirb/IR.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <gtirb/proto/Module.pb.h>
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
  if (!setNodeUUIDFromBytes(M, Message.uuid()))
    return nullptr;
  M->BinaryPath = Message.binary_path();
  M->PreferredAddr = Addr(Message.preferred_addr());
  M->RebaseDelta = Message.rebase_delta();
  M->FileFormat = static_cast<gtirb::FileFormat>(Message.file_format());
  M->Isa = static_cast<ISA>(Message.isa());
  M->Name = Message.name();
  for (const auto& Elt : Message.proxies()) {
    auto* PB = ProxyBlock::fromProtobuf(C, M, Elt);
    if (!PB)
      return nullptr;
    M->ProxyBlocks.insert(PB);
    if (Parent) {
      addVertex(PB, Parent->getCFG());
    }
  }
  for (const auto& Elt : Message.sections()) {
    auto* S = Section::fromProtobuf(C, M, Elt);
    if (!S)
      return nullptr;
    M->Sections.emplace(S);
    S->addToIndices();
  }
  for (const auto& Elt : Message.symbols()) {
    auto* S = Symbol::fromProtobuf(C, M, Elt);
    if (!S)
      return nullptr;
    M->Symbols.emplace(S);
  }
  for (const auto& ProtoS : Message.sections()) {
    for (const auto& ProtoBI : ProtoS.byte_intervals()) {
      UUID Id;
      if (!uuidFromBytes(ProtoBI.uuid(), Id))
        return nullptr;
      auto* BI = dyn_cast_or_null<ByteInterval>(getByUUID(C, Id));
      if (!BI)
        return nullptr;
      if (!BI->symbolicExpressionsFromProtobuf(C, ProtoBI))
        return nullptr;
    }
  }
  if (!Message.entry_point().empty()) {
    UUID Id;
    if (!uuidFromBytes(Message.entry_point(), Id))
      return nullptr;
    M->EntryPoint = dyn_cast_or_null<CodeBlock>(Node::getByUUID(C, Id));
    if (!M->EntryPoint)
      return nullptr;
  }
  static_cast<AuxDataContainer*>(M)->fromProtobuf(Message);
  return M;
}

void Module::moveProxyBlock(ProxyBlock* B) {
  if (B->getModule()) {
    B->getModule()->removeProxyBlock(B);
  }
  ProxyBlocks.insert(B);
  B->setModule(this);
  if (Parent) {
    addVertex(B, Parent->getCFG());
  }
}

void Module::removeProxyBlock(ProxyBlock* B) {
  ProxyBlocks.erase(B);
  B->setModule(nullptr);
  if (Parent) {
    removeVertex(B, Parent->getCFG());
  }
}

ProxyBlock* Module::addProxyBlock(ProxyBlock* B) {
  if (Module* M = B->getModule()) {
    M->removeProxyBlock(B);
  }

  ProxyBlocks.insert(B);
  B->setModule(this);
  if (Parent) {
    addVertex(B, Parent->getCFG());
  }
  return B;
}

// Present for testing purposes only.
void Module::save(std::ostream& Out) const {
  MessageType Message;
  this->toProtobuf(&Message);
  Message.SerializeToOstream(&Out);
}

// Present for testing purposes only.
Module* Module::load(Context& C, std::istream& In) {
  return load(C, nullptr, In);
}

// Present for testing purposes only.
Module* Module::load(Context& C, IR* Parent, std::istream& In) {
  MessageType Message;
  Message.ParseFromIstream(&In);
  auto M = Module::fromProtobuf(C, Parent, Message);
  return M;
}
