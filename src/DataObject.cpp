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
