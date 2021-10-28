//===- ProxyBlock.cpp -------------------------------------------*- C++ -*-===//
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
#include "ProxyBlock.hpp"
#include "IR.hpp"
#include "Serialization.hpp"
#include <gtirb/proto/ProxyBlock.pb.h>

using namespace gtirb;

void ProxyBlock::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
}

Expected<ProxyBlock*> ProxyBlock::fromProtobuf(Context& C,
                                               const MessageType& Message) {
  UUID Id;
  if (!uuidFromBytes(Message.uuid(), Id))
    return createStringError(IR::load_error::CorruptFile,
                             "Could not create proxy block");

  return Create(C, Id);
}

// Present for testing purposes only.
void ProxyBlock::save(std::ostream& Out) const {
  MessageType Message;
  this->toProtobuf(&Message);
  Message.SerializeToOstream(&Out);
}

// Present for testing purposes only.
ProxyBlock* ProxyBlock::load(Context& C, std::istream& In) {
  MessageType Message;
  Message.ParseFromIstream(&In);
  auto CB = ProxyBlock::fromProtobuf(C, Message);
  if (CB)
    return *CB;
  consumeError(CB.takeError());
  return nullptr;
}
