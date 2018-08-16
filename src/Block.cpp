#include "Block.hpp"
#include "Serialization.hpp"
#include <proto/Block.pb.h>
#include <proto/InstructionRef.pb.h>

using namespace gtirb;

EA Block::getAddress() const { return this->Address; }

uint64_t Block::getSize() const { return this->Size; }

uint64_t Block::getDecodeMode() const { return this->DecodeMode; }

void Block::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_address(this->Address);
  Message->set_size(this->Size);
  Message->set_decode_mode(this->DecodeMode);
}

Block* Block::fromProtobuf(Context& C, const MessageType& Message) {
  auto *B = Block::Create(C, EA(Message.address()), EA(Message.size()),
                          Message.decode_mode());
  setNodeUUIDFromBytes(B, Message.uuid());
  return B;
}

void InstructionRef::toProtobuf(MessageType* Message) const {
  uuidToBytes(this->BlockRef.getUUID(), *Message->mutable_block_id());
  Message->set_offset(this->Offset);
}

void InstructionRef::fromProtobuf(Context &, const MessageType& Message) {
  this->BlockRef = uuidFromBytes(Message.block_id());
  this->Offset = Message.offset();
}
