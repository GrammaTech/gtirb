//===- Offset.cpp -----------------------------------------------*- C++ -*-===//
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
#include <gtirb/Offset.hpp>
#include <gtirb/proto/Offset.pb.h>

using namespace gtirb;

void Offset::toProtobuf(MessageType* Message) const {
  uuidToBytes(this->ElementId, *Message->mutable_element_id());
  Message->set_displacement(this->Displacement);
}

bool Offset::fromProtobuf(Context&, const MessageType& Message) {
  if (!uuidFromBytes(Message.element_id(), this->ElementId))
    return false;
  this->Displacement = Message.displacement();
  return true;
}
