#include "Block.hpp"
#include "Serialization.hpp"
#include <proto/Block.pb.h>
#include <proto/InstructionRef.pb.h>

using namespace gtirb;

void Block::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_address(static_cast<uint64_t>(this->Address));
  Message->set_size(this->Size);
  Message->set_decode_mode(this->DecodeMode);
}

Block* Block::fromProtobuf(Context& C, const MessageType& Message) {
  auto *B = Block::Create(C, Addr(Message.address()), Addr(Message.size()),
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
