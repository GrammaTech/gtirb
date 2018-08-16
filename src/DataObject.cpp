#include "DataObject.hpp"
#include "Serialization.hpp"
#include <proto/DataObject.pb.h>

using namespace gtirb;

EA DataObject::getAddress() const { return this->Address; }

uint64_t DataObject::getSize() const { return this->Size; }

void DataObject::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_address(this->Address);
  Message->set_size(this->Size);
}

DataObject *DataObject::fromProtobuf(Context &C, const MessageType& Message) {
  auto *DO = DataObject::Create(C, EA(Message.address()), Message.size());
  setNodeUUIDFromBytes(DO, Message.uuid());
  return DO;
}
