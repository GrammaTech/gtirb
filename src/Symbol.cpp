#include "Symbol.hpp"
#include "Serialization.hpp"
#include <gtirb/Block.hpp>
#include <gtirb/DataObject.hpp>

using namespace gtirb;

Symbol::Symbol(EA X, std::string Name_, const DataObject& DO,
               StorageKind StorageKind_)
    : Symbol(X, Name_, StorageKind_) {
  this->setReferent(DO);
}

Symbol::Symbol(EA X, std::string Name_, const Block& B,
               StorageKind StorageKind_)
    : Symbol(X, Name_, StorageKind_) {
  this->setReferent(B);
}

void Symbol::setEA(gtirb::EA X) { this->Ea = X; }

gtirb::EA Symbol::getEA() const { return this->Ea; }

void Symbol::setName(std::string X) { this->Name = X; }

std::string Symbol::getName() const { return this->Name; }

void Symbol::setReferent(const DataObject& Data) {
  this->Referent = NodeRef<DataObject>(Data);
}

void Symbol::setReferent(const Block& Instruction) {
  this->Referent = NodeRef<Block>(Instruction);
}

NodeRef<DataObject> Symbol::getDataReferent() const {
  if (auto *P = std::get_if<NodeRef<DataObject>>(&this->Referent)) {
    return *P;
  }
  return NodeRef<DataObject>();
}

NodeRef<Block> Symbol::getCodeReferent() const {
  if (auto *P = std::get_if<NodeRef<Block>>(&this->Referent)) {
    return *P;
  }
  return NodeRef<Block>();
}

void Symbol::setStorageKind(Symbol::StorageKind X) { this->Storage = X; }

gtirb::Symbol::StorageKind Symbol::getStorageKind() const {
  return this->Storage;
}

namespace {
template <typename... Ts> struct overload : Ts... {
  using Ts::operator()...;
};
template <typename... Ts> overload(Ts...) -> overload<Ts...>;
}

void Symbol::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_ea(this->Ea);
  Message->set_name(this->Name);
  Message->set_storage_kind(static_cast<proto::StorageKind>(this->Storage));
  std::visit(overload{
      [Message](const NodeRef<Block> &B) {
        uuidToBytes(B.getUUID(), *Message->mutable_code_referent_uuid());
      },
      [Message](const NodeRef<DataObject> &DO) {
        uuidToBytes(DO.getUUID(), *Message->mutable_data_referent_uuid());
      }},
      this->Referent);
}

Symbol* Symbol::fromProtobuf(Context& C, const MessageType& Message) {
  Symbol* S = Symbol::Create(C, EA(Message.ea()), Message.name(),
                             static_cast<StorageKind>(Message.storage_kind()));
  setNodeUUIDFromBytes(S, Message.uuid());
  if (!Message.code_referent_uuid().empty())
    S->Referent = NodeRef<Block>(uuidFromBytes(Message.code_referent_uuid()));
  else if (!Message.data_referent_uuid().empty())
    S->Referent =
        NodeRef<DataObject>(uuidFromBytes(Message.data_referent_uuid()));
  return S;
}
