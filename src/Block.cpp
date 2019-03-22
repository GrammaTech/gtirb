//===- Block.cpp ------------------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2018 GrammaTech, Inc.
//
//  This code is licensed under the MIT license. See the LICENSE file in the
//  project root for license terms.
//
//  This project is sponsored by the Office of Naval Research, One Liberty
//  Center, 875 N. Randolph Street, Arlington, VA 22203 under contract #
//  N68335-17-C-0700.  The content of the information does not necessarily
//  reflect the position or policy of the Government and no official
//  endorsement should be inferred.
//
//===----------------------------------------------------------------------===//
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

// Note: in order to handle vertex descriptors correctly, Block
// deserialization is done by CFG::fromProtobuf.

void InstructionRef::toProtobuf(MessageType* Message) const {
  uuidToBytes(this->BlockId, *Message->mutable_block_id());
  Message->set_offset(this->Offset);
}

void InstructionRef::fromProtobuf(Context&, const MessageType& Message) {
  this->BlockId = uuidFromBytes(Message.block_id());
  this->Offset = Message.offset();
}
