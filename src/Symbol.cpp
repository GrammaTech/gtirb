//===- Symbol.cpp -----------------------------------------------*- C++ -*-===//
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
#include "Symbol.hpp"
#include "Serialization.hpp"
#include <gtirb/Block.hpp>
#include <gtirb/DataObject.hpp>

using namespace gtirb;

void Symbol::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  if (this->Address) {
    Message->set_address(static_cast<uint64_t>(this->Address.value()));
  } else {
    Message->clear_optional_address();
  }
  Message->set_name(this->Name);
  Message->set_storage_kind(static_cast<proto::StorageKind>(this->Storage));
  if (this->Referent) {
    nodeUUIDToBytes(this->Referent, *Message->mutable_referent_uuid());
  }
}

Symbol* Symbol::fromProtobuf(Context& C, const MessageType& Message) {
  Symbol* S;

  if (Message.optional_address_case() == proto::Symbol::kAddress) {
    S = Symbol::Create(C, Addr(Message.address()), Message.name());
  } else {
    S = Symbol::Create(C, Message.name());
  }
  S->setStorageKind(static_cast<StorageKind>(Message.storage_kind()));
  setNodeUUIDFromBytes(S, Message.uuid());

  if (!Message.referent_uuid().empty()) {
    S->Referent = Node::getByUUID(C, uuidFromBytes(Message.referent_uuid()));
  }

  return S;
}
