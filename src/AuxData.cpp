//===- AuxData.cpp ---------------------------------------------*- C++-*-===//
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
#include "AuxData.hpp"
#include <gtirb/proto/AuxData.pb.h>

namespace gtirb {
void AuxData::fromProtobuf(AuxData& Result, const MessageType& Message) {
  Result.SF.ProtobufType = Message.type_name();
  Result.SF.RawBytes = Message.data();
}

void AuxData::toProtobuf(MessageType* Message,
                         const AuxData::SerializedForm& SFToSerialize) const {
  *Message->mutable_type_name() = SFToSerialize.ProtobufType;
  *Message->mutable_data() = SFToSerialize.RawBytes;
}

bool AuxData::checkAuxDataMessageType(const AuxData::MessageType& Message,
                                      const std::string& ExpectedName) {
  return Message.type_name() == ExpectedName;
}

// Present for testing purposes only.
void AuxData::save(std::ostream& Out) const {
  MessageType Message;
  this->toProtobuf(&Message);
  Message.SerializeToOstream(&Out);
}

// Present for testing purposes only.
std::unique_ptr<AuxData>
AuxData::load(std::istream& In,
              std::unique_ptr<AuxData> (*FPPtr)(const MessageType&)) {
  MessageType Message;
  Message.ParseFromIstream(&In);
  auto AD = FPPtr(Message);
  return AD;
}
} // namespace gtirb
