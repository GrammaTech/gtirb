#include "Instruction.hpp"
#include "Serialization.hpp"
#include <proto/Instruction.pb.h>

using namespace gtirb;

Instruction::Instruction(EA address_, uint64_t size_) : Node(), address(address_), size(size_) {}

void Instruction::setAddress(gtirb::EA x) { this->address = x; }

gtirb::EA Instruction::getAddress() const { return this->address; }

uint64_t Instruction::getSize() const { return this->size; }

void Instruction::toProtobuf(MessageType* message) const {
  nodeUUIDToBytes(this, *message->mutable_uuid());
  message->set_address(this->address);
  message->set_size(this->size);
}

void Instruction::fromProtobuf(const MessageType& message) {
  setNodeUUIDFromBytes(this, message.uuid());
  this->address = EA(message.address());
  this->size = message.size();
}
