#include "DataObject.hpp"
#include "Serialization.hpp"
#include <proto/DataObject.pb.h>

using namespace gtirb;

EA DataObject::getAddress() const { return this->address; }

uint64_t DataObject::getSize() const { return this->size; }

void DataObject::toProtobuf(MessageType* message) const {
  nodeUUIDToBytes(this, *message->mutable_uuid());
  message->set_address(this->address);
  message->set_size(this->size);
}

void DataObject::fromProtobuf(const MessageType& message) {
  setNodeUUIDFromBytes(this, message.uuid());
  this->address = EA(message.address());
  this->size = message.size();
}
