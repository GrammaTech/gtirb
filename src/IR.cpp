//===- IR.cpp ---------------------------------------------------*- C++ -*-===//
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
#include "IR.hpp"
#include "Serialization.hpp"
#include <gtirb/DataObject.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <proto/IR.pb.h>
#include <google/protobuf/util/json_util.h>

using namespace gtirb;

void IR::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  containerToProtobuf(this->Modules, Message->mutable_modules());

  AuxDataContainer::toProtobuf(Message->mutable_aux_data_container());
}

IR* IR::fromProtobuf(Context& C, const MessageType& Message) {
  auto* I = IR::Create(C);
  setNodeUUIDFromBytes(I, Message.uuid());
  containerFromProtobuf(C, I->Modules, Message.modules());

  AuxDataContainer::fromProtobuf(static_cast<AuxDataContainer*>(I), C,
                                 Message.aux_data_container());
  return I;
}

void IR::save(std::ostream& Out) const {
  MessageType Message;
  this->toProtobuf(&Message);
  Message.SerializeToOstream(&Out);
}

IR* IR::load(Context& C, std::istream& In) {
  MessageType Message;
  Message.ParseFromIstream(&In);
  return IR::fromProtobuf(C, Message);
}

void IR::saveJSON(std::ostream& Out) const {
  MessageType Message;
  this->toProtobuf(&Message);
  std::string S;
  google::protobuf::util::MessageToJsonString(Message, &S);
  Out << S;
}

IR* IR::loadJSON(Context& C, std::istream& In) {
  MessageType Message;
  std::string S;
  google::protobuf::util::JsonStringToMessage(
      std::string(std::istreambuf_iterator<char>(In), {}), &Message);
  return IR::fromProtobuf(C, Message);
}
