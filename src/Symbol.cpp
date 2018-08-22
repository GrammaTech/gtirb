#include "Symbol.hpp"
#include "Serialization.hpp"
#include <gtirb/Block.hpp>
#include <gtirb/DataObject.hpp>

using namespace gtirb;

Symbol::Symbol(EA X, std::string Name_, const DataObject& Referent,
               StorageKind StorageKind_)
    : Symbol(X, Name_, StorageKind_) {
  this->setReferent(Referent);
}

Symbol::Symbol(EA X, std::string Name_, const Block& Referent,
               StorageKind StorageKind_)
    : Symbol(X, Name_, StorageKind_) {
  this->setReferent(Referent);
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
  return std::get<NodeRef<DataObject>>(this->Referent);
}

NodeRef<Block> Symbol::getCodeReferent() const {
  return std::get<NodeRef<Block>>(this->Referent);
}

void Symbol::setStorageKind(Symbol::StorageKind X) { this->Storage = X; }

gtirb::Symbol::StorageKind Symbol::getStorageKind() const {
  return this->Storage;
}

void Symbol::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_ea(this->Ea);
  Message->set_name(this->Name);
  Message->set_storage_kind(static_cast<proto::StorageKind>(this->Storage));
  std::visit(
      [Message](auto& Arg) {
        using T = std::decay_t<decltype(Arg)>;
        if constexpr (std::is_same_v<T, NodeRef<Block>>) {
          uuidToBytes(Arg.getUUID(), *Message->mutable_code_referent_uuid());
        } else if constexpr (std::is_same_v<T, NodeRef<DataObject>>) {
          uuidToBytes(Arg.getUUID(), *Message->mutable_data_referent_uuid());
        } else {
          static_assert(false, "Unknown symbol referent");
        }
      },
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
