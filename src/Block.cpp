#include "Block.hpp"
#include "Serialization.hpp"
#include <proto/Block.pb.h>

using namespace gtirb;

Block::Block(EA address_, uint64_t size_) : Node(), address(address_), size(size_) {}

EA Block::getAddress() const { return this->address; }
uint64_t Block::getSize() const { return this->size; }

void Block::toProtobuf(MessageType* message) const {
  nodeUUIDToBytes(this, *message->mutable_uuid());
  message->set_address(this->address);
  message->set_size(this->size);
}

void Block::fromProtobuf(const MessageType& message) {
  setNodeUUIDFromBytes(this, message.uuid());
  this->address = EA(message.address());
  this->size = EA(message.size());
}
