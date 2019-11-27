//===- DataBlock.cpp -------------------------------------------*- C++ -*-===//
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
#include "DataBlock.hpp"
#include <gtirb/DataBlock.hpp>
#include <gtirb/Serialization.hpp>

#include <proto/DataBlock.pb.h>

using namespace gtirb;

void DataBlock::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_size(this->Size);
}

DataBlock* DataBlock::fromProtobuf(Context& C, const MessageType& Message) {
  auto* DO = DataBlock::Create(C, Message.size());
  setNodeUUIDFromBytes(DO, Message.uuid());
  return DO;
}
