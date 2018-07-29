#include <gtirb/Data.hpp>
#include <gtirb/Instruction.hpp>
#include <gtirb/Symbol.hpp>
#include "Serialization.hpp"

using namespace gtirb;

Symbol::Symbol(EA x) : Node(), ea(x) {}

Symbol::Symbol(EA x, std::string name_, StorageKind storageKind_)
    : Node(), ea(x), name(name_), storageKind(storageKind_) {}

Symbol::Symbol(EA x, std::string name_, const Data& referent, StorageKind storageKind_)
    : Symbol(x, name_, storageKind_) {
  this->setReferent(referent);
}

Symbol::Symbol(EA x, std::string name_, const Instruction& referent, StorageKind storageKind_)
    : Symbol(x, name_, storageKind_) {
  this->setReferent(referent);
}

void Symbol::setEA(gtirb::EA x) { this->ea = x; }

gtirb::EA Symbol::getEA() const { return this->ea; }

void Symbol::setName(std::string x) { this->name = x; }

std::string Symbol::getName() const { return this->name; }

void Symbol::setReferent(const Data& data) {
  this->dataReferent = NodeRef<Data>(data);
  this->codeReferent = {};
}

void Symbol::setReferent(const Instruction& instruction) {
  this->codeReferent = NodeRef<Instruction>(instruction);
  this->dataReferent = {};
}

NodeRef<Data> Symbol::getDataReferent() const { return this->dataReferent; }

NodeRef<Instruction> Symbol::getCodeReferent() const { return this->codeReferent; }

void Symbol::setStorageKind(Symbol::StorageKind x) { this->storageKind = x; }

gtirb::Symbol::StorageKind Symbol::getStorageKind() const { return this->storageKind; }

void Symbol::toProtobuf(MessageType* message) const {
  nodeUUIDToBytes(this, *message->mutable_uuid());
  message->set_ea(this->ea);
  message->set_name(this->name);
  message->set_storage_kind(static_cast<proto::StorageKind>(this->storageKind));
  uuidToBytes(this->codeReferent.getUUID(), *message->mutable_code_referent_uuid());
  uuidToBytes(this->dataReferent.getUUID(), *message->mutable_data_referent_uuid());
}

void Symbol::fromProtobuf(const MessageType& message) {
  setNodeUUIDFromBytes(this, message.uuid());
  this->ea = EA(message.ea());
  this->name = message.name();
  this->storageKind = static_cast<StorageKind>(message.storage_kind());
  this->codeReferent = {uuidFromBytes(message.code_referent_uuid())};
  this->dataReferent = {uuidFromBytes(message.data_referent_uuid())};
}
