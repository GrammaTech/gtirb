//===- DataBlock.cpp -------------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2020 GrammaTech, Inc.
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
#include "Serialization.hpp"
#include <gtirb/ByteInterval.hpp>
#include <gtirb/DataBlock.hpp>
#include <gtirb/proto/DataBlock.pb.h>

using namespace gtirb;

void DataBlock::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_size(this->Size);
}

DataBlock* DataBlock::fromProtobuf(Context& C, ByteInterval* Parent,
                                   const MessageType& Message) {
  // Because we do not have an offset, we cannot create the data block and
  // set its parent at the same time.
  auto* DO = DataBlock::Create(C, Message.size());
  DO->setByteInterval(Parent);
  if (!setNodeUUIDFromBytes(DO, Message.uuid()))
    return nullptr;
  return DO;
}

uint64_t DataBlock::getOffset() const {
  assert(Parent &&
         "invalid call to DataBlock::getOffset: Parent must not be null!");
  return Parent->nodeToBlock(this).getOffset();
}

std::optional<Addr> DataBlock::getAddress() const {
  if (!Parent) {
    return std::nullopt;
  }
  if (auto BaseAddr = Parent->getAddress()) {
    return *BaseAddr + getOffset();
  }
  return std::nullopt;
}

// Present for testing purposes only.
void DataBlock::save(std::ostream& Out) const {
  MessageType Message;
  this->toProtobuf(&Message);
  Message.SerializeToOstream(&Out);
}

// Present for testing purposes only.
DataBlock* DataBlock::load(Context& C, std::istream& In) {
  MessageType Message;
  Message.ParseFromIstream(&In);
  auto DB = DataBlock::fromProtobuf(C, nullptr, Message);
  return DB;
}
