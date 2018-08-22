#include "DataObject.hpp"
#include "Serialization.hpp"
#include <proto/DataObject.pb.h>

using namespace gtirb;

Addr DataObject::getAddress() const { return this->Address; }

uint64_t DataObject::getSize() const { return this->Size; }

void DataObject::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_address(static_cast<uint64_t>(this->Address));
  Message->set_size(this->Size);
}

void DataObject::fromProtobuf(const MessageType& Message) {
  setNodeUUIDFromBytes(this, Message.uuid());
  this->Address = Addr(Message.address());
  this->Size = Message.size();
}
