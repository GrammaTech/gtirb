#include "Block.hpp"
#include "Serialization.hpp"
#include <proto/Block.pb.h>
#include <proto/InstructionRef.pb.h>

using namespace gtirb;

Block::Block(EA Address_, uint64_t Size_, uint64_t DecodeMode_)
    : Node(), Address(Address_), Size(Size_), DecodeMode(DecodeMode_) {}

EA Block::getAddress() const { return this->Address; }

uint64_t Block::getSize() const { return this->Size; }

uint64_t Block::getDecodeMode() const { return this->DecodeMode; }

void Block::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_address(this->Address);
  Message->set_size(this->Size);
  Message->set_decode_mode(this->DecodeMode);
}

void Block::fromProtobuf(const MessageType& Message) {
  setNodeUUIDFromBytes(this, Message.uuid());
  this->Address = EA(Message.address());
  this->Size = EA(Message.size());
  this->DecodeMode = Message.decode_mode();
}

void InstructionRef::toProtobuf(MessageType* Message) const {
  uuidToBytes(this->BlockRef.getUUID(), *Message->mutable_block_id());
  Message->set_offset(this->Offset);
}

void InstructionRef::fromProtobuf(const MessageType& Message) {
  this->BlockRef = uuidFromBytes(Message.block_id());
  this->Offset = Message.offset();
}
