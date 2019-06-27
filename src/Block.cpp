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
#include <proto/Offset.pb.h>

using namespace gtirb;

void Block::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_address(static_cast<uint64_t>(this->Address));
  Message->set_size(this->Size);
  Message->set_decode_mode(this->DecodeMode);
}

Block* Block::fromProtobuf(Context& C, const proto::Block& M) {
  Block* B = Block::Create(C, Addr(M.address()), M.size(), M.decode_mode());
  setNodeUUIDFromBytes(B, M.uuid());
  return B;
}

void Offset::toProtobuf(MessageType* Message) const {
  uuidToBytes(this->ElementId, *Message->mutable_element_id());
  Message->set_offset(this->Offset);
}

void Offset::fromProtobuf(Context&, const MessageType& Message) {
  this->ElementId = uuidFromBytes(Message.element_id());
  this->Offset = Message.offset();
}
