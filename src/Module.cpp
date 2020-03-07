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

Module* Module::fromProtobuf(Context& C, const MessageType& Message) {
  Module* M = Module::Create(C);
  if (!setNodeUUIDFromBytes(M, Message.uuid()))
    return nullptr;
  M->BinaryPath = Message.binary_path();
  M->PreferredAddr = Addr(Message.preferred_addr());
  M->RebaseDelta = Message.rebase_delta();
  M->FileFormat = static_cast<gtirb::FileFormat>(Message.file_format());
  M->Isa = static_cast<ISA>(Message.isa());
  M->Name = Message.name();
  for (const auto& Elt : Message.proxies()) {
    auto* PB = ProxyBlock::fromProtobuf(C, Elt);
    if (!PB)
      return nullptr;
    M->ProxyBlocks.insert(PB);
  }
  for (const auto& Elt : Message.sections()) {
    auto* S = Section::fromProtobuf(C, Elt);
    if (!S)
      return nullptr;
    M->addSection(S);
  }
  for (const auto& Elt : Message.symbols()) {
    auto* S = Symbol::fromProtobuf(C, Elt);
    if (!S)
      return nullptr;
    M->addSymbol(S);
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

void Module::removeProxyBlock(ProxyBlock* B) {
  if (auto It = ProxyBlocks.find(B); It != ProxyBlocks.end()) {
    if (Parent) {
      Parent->removeProxyBlocks(this,
                                boost::make_iterator_range(It, std::next(It)));
    }
    ProxyBlocks.erase(It);
    B->setModule(nullptr);
  }
}

ProxyBlock* Module::addProxyBlock(ProxyBlock* B) {
  if (Module* M = B->getModule()) {
    M->removeProxyBlock(B);
  }

  B->setModule(this);
  ProxyBlocks.insert(B);
  if (Parent) {
    auto It = ProxyBlocks.find(B);
    Parent->addProxyBlocks(this, boost::make_iterator_range(It, std::next(It)));
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
  MessageType Message;
  Message.ParseFromIstream(&In);
  auto M = Module::fromProtobuf(C, Message);
  return M;
}

bool Module::removeSection(Section* S) {
  S->removeFromIndices();
  auto& Index = Sections.get<by_pointer>();
  if (auto Iter = Index.find(S); Iter != Index.end()) {
    if (Parent) {
      auto Begin = Sections.project<by_address>(Iter);
      auto End = std::next(Begin);
      Parent->removeCodeBlocks(this, makeCodeBlockRange(Begin, End));
    }
    Index.erase(Iter);
    S->setModule(nullptr);
    return true;
  }
  return false;
}

Section* Module::addSection(Section* S) {
  if (S->getModule()) {
    S->getModule()->removeSection(S);
  }
  S->setModule(this);
  auto [Iter, Inserted] = Sections.emplace(S);
  if (Inserted && Parent) {
    Parent->addCodeBlocks(this, makeCodeBlockRange(Iter, std::next(Iter)));
  }
  S->addToIndices();
  return S;
}
