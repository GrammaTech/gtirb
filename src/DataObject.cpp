//===- DataObject.cpp -------------------------------------------*- C++ -*-===//
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
#include "DataObject.hpp"
#include "Serialization.hpp"
#include <proto/DataObject.pb.h>

using namespace gtirb;

void DataObject::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_address(static_cast<uint64_t>(this->Address));
  Message->set_size(this->Size);
}

DataObject* DataObject::fromProtobuf(Context& C, const MessageType& Message) {
  auto* DO = DataObject::Create(C, Addr(Message.address()), Message.size());
  setNodeUUIDFromBytes(DO, Message.uuid());
  return DO;
}
