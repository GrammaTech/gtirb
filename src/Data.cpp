#include "Data.hpp"
#include "Serialization.hpp"
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Module.hpp>
#include <proto/Data.pb.h>

using namespace gtirb;

EA Data::getAddress() const { return this->address; }

uint64_t Data::getSize() const { return this->size; }

std::vector<uint8_t> Data::getBytes(const Module& module) const {
  return module.getImageByteMap().getData(this->getAddress(), this->getSize());
}

void Data::toProtobuf(MessageType* message) const {
  nodeUUIDToBytes(this, *message->mutable_uuid());
  message->set_address(this->address);
  message->set_size(this->size);
}

void Data::fromProtobuf(const MessageType& message) {
  setNodeUUIDFromBytes(this, message.uuid());
  this->address = EA(message.address());
  this->size = message.size();
}
