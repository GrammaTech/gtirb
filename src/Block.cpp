#include "Block.hpp"
#include "Serialization.hpp"
#include <gtirb/Instruction.hpp>
#include <proto/Block.pb.h>

using namespace gtirb;

Block::Block(EA address_, uint64_t size_) : Node(), address(address_), size(size_) {}

Block::Block(EA address_, uint64_t size_, std::vector<Instruction>&& instructions_)
    : Node(), address(address_), size(size_), instructions(std::move(instructions_)) {}

EA Block::getAddress() const { return this->address; }
uint64_t Block::getSize() const { return this->size; }

std::vector<Instruction>& Block::getInstructions() { return this->instructions; }

const std::vector<Instruction>& Block::getInstructions() const { return this->instructions; }

void Block::toProtobuf(MessageType* message) const {
  nodeUUIDToBytes(this, *message->mutable_uuid());
  message->set_address(this->address);
  message->set_size(this->size);
  containerToProtobuf(this->instructions, message->mutable_instructions());
}

void Block::fromProtobuf(const MessageType& message) {
  setNodeUUIDFromBytes(this, message.uuid());
  this->address = EA(message.address());
  this->size = EA(message.size());
  containerFromProtobuf(this->instructions, message.instructions());
}
