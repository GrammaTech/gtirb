#include "Symbol.hpp"
#include "Serialization.hpp"
#include <gtirb/Block.hpp>
#include <gtirb/DataObject.hpp>

using namespace gtirb;

Symbol::Symbol(Addr X) : Node(), Address(X) {}

Symbol::Symbol(Addr X, std::string Name_, StorageKind StorageKind_)
    : Node(), Address(X), Name(Name_), Storage(StorageKind_) {}

Symbol::Symbol(Addr X, std::string Name_, const DataObject& Referent,
               StorageKind StorageKind_)
    : Symbol(X, Name_, StorageKind_) {
  this->setReferent(Referent);
}

Symbol::Symbol(Addr X, std::string Name_, const Block& Referent,
               StorageKind StorageKind_)
    : Symbol(X, Name_, StorageKind_) {
  this->setReferent(Referent);
}

void Symbol::setReferent(const DataObject& Data) {
  this->DataReferent = NodeRef<DataObject>(Data);
  this->CodeReferent = {};
}

void Symbol::setReferent(const Block& Instruction) {
  this->CodeReferent = NodeRef<Block>(Instruction);
  this->DataReferent = {};
}

void Symbol::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_address(static_cast<uint64_t>(this->Address));
  Message->set_name(this->Name);
  Message->set_storage_kind(static_cast<proto::StorageKind>(this->Storage));
  uuidToBytes(this->CodeReferent.getUUID(),
              *Message->mutable_code_referent_uuid());
  uuidToBytes(this->DataReferent.getUUID(),
              *Message->mutable_data_referent_uuid());
}

void Symbol::fromProtobuf(const MessageType& Message) {
  setNodeUUIDFromBytes(this, Message.uuid());
  this->Address = Addr(Message.address());
  this->Name = Message.name();
  this->Storage = static_cast<StorageKind>(Message.storage_kind());
  this->CodeReferent = {uuidFromBytes(Message.code_referent_uuid())};
  this->DataReferent = {uuidFromBytes(Message.data_referent_uuid())};
}
