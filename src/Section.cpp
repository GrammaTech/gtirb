//===- Section.cpp ----------------------------------------------*- C++ -*-===//
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
#include "Section.hpp"
#include <gtirb/Serialization.hpp>
#include <proto/Section.pb.h>

using namespace gtirb;

bool Section::operator==(const Section& Other) const {
  return this->getAddress() == Other.getAddress() &&
         this->getSize() == Other.getSize() && this->Name == Other.Name;
}

bool Section::operator!=(const Section& Other) const {
  return !(*this == Other);
}

void Section::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_name(this->Name);
  for (const auto& Interval : byte_intervals()) {
    Interval.toProtobuf(Message->add_byte_intervals());
  }
}

Section* Section::fromProtobuf(Context& C, Module* Parent,
                               const MessageType& Message) {
  auto* S = Section::Create(C, Parent, Message.name());
  setNodeUUIDFromBytes(S, Message.uuid());
  for (const auto& ProtoInterval : Message.byte_intervals()) {
    auto* BI = ByteInterval::fromProtobuf(C, S, ProtoInterval);
    BI->addToIndices();
    S->mutateIndices([S, BI]() { S->ByteIntervals.emplace(BI); });
  }
  return S;
}
