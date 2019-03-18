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

class StorePayload {
public:
  StorePayload(Symbol::MessageType* Message) : M(Message) {}
  void operator()(std::monostate) const { M->clear_value(); }
  void operator()(uint64_t X) const { M->set_value(X); }
  void operator()(const Node* Referent) const {
    nodeUUIDToBytes(Referent, *M->mutable_referent_uuid());
  }

private:
  Symbol::MessageType* M;
};

void Symbol::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  std::visit(StorePayload(Message), Payload);
  Message->set_name(this->Name);
  Message->set_storage_kind(static_cast<proto::StorageKind>(this->Storage));
}

Symbol* Symbol::fromProtobuf(Context& C, const MessageType& Message) {
  Symbol* S = Symbol::Create(C, Message.name());
  switch (Message.optional_payload_case()) {
  case proto::Symbol::kValue:
    S->Payload = Message.value();
    break;
  case proto::Symbol::kReferentUuid:
    S->Payload = Node::getByUUID(C, uuidFromBytes(Message.referent_uuid()));
    break;
  default:
      /* nothing to do */;
  }
  S->setStorageKind(static_cast<StorageKind>(Message.storage_kind()));
  setNodeUUIDFromBytes(S, Message.uuid());

  return S;
}
