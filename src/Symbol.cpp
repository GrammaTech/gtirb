#include "Symbol.hpp"
#include "Serialization.hpp"
#include <gtirb/Block.hpp>
#include <gtirb/DataObject.hpp>

using namespace gtirb;

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

Symbol *Symbol::fromProtobuf(Context &C, const MessageType& Message) {
  Symbol* S = Symbol::Create(C, EA(Message.address()), Message.name(),
                             static_cast<StorageKind>(Message.storage_kind()));
  setNodeUUIDFromBytes(S, Message.uuid());
  S->CodeReferent = {uuidFromBytes(Message.code_referent_uuid())};
  S->DataReferent = {uuidFromBytes(Message.data_referent_uuid())};
  return S;
}
