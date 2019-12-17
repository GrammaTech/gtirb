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
#include <gtirb/ByteInterval.hpp>
#include <gtirb/CodeBlock.hpp>
#include <gtirb/Serialization.hpp>
#include <proto/Offset.pb.h>

using namespace gtirb;

void CodeBlock::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_size(this->Size);
  Message->set_decode_mode(this->DecodeMode);
}

CodeBlock* CodeBlock::fromProtobuf(Context& C, ByteInterval* Parent,
                                   const MessageType& Message) {
  CodeBlock* B =
      CodeBlock::Create(C, Parent, Message.size(), Message.decode_mode());
  setNodeUUIDFromBytes(B, Message.uuid());
  return B;
}

void Offset::toProtobuf(MessageType* Message) const {
  uuidToBytes(this->ElementId, *Message->mutable_element_id());
  Message->set_displacement(this->Displacement);
}

void Offset::fromProtobuf(Context&, const MessageType& Message) {
  this->ElementId = uuidFromBytes(Message.element_id());
  this->Displacement = Message.displacement();
}

uint64_t CodeBlock::getOffset() const {
  assert(Parent &&
         "invalid call to CodeBlock::getOffset: Parent must not be null!");
  return Parent->nodeToBlock(this).getOffset();
}

std::optional<Addr> CodeBlock::getAddress() const {
  if (!Parent)
    return std::nullopt;
  if (auto baseAddr = Parent->getAddress())
    return *baseAddr + getOffset();
  return std::nullopt;
}
