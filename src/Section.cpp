//===- Section.cpp ----------------------------------------------*- C++ -*-===//
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
#include "Section.hpp"
#include "Serialization.hpp"
#include <proto/Section.pb.h>

using namespace gtirb;

bool Section::operator==(const Section& Other) const {
  return this->Address == Other.Address && this->Size == Other.Size &&
         this->Name == Other.Name;
}

bool Section::operator!=(const Section& Other) const {
  return !(*this == Other);
}

void Section::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_name(this->Name);
  Message->set_size(this->Size);
  Message->set_address(static_cast<uint64_t>(this->Address));
}

Section* Section::fromProtobuf(Context& C, const MessageType& Message) {
  auto* S = Section::Create(C, Message.name(), Addr(Message.address()),
                            Message.size());
  setNodeUUIDFromBytes(S, Message.uuid());
  return S;
}
