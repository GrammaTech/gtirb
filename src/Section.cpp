#include <proto/Section.pb.h>
#include <gtirb/Section.hpp>
#include <gtirb/Serialization.hpp>

using namespace gtirb;

Section::Section(std::string n, EA ea, uint64_t s) : Node(), name(n), address(ea), size(s) {}

const std::string& Section::getName() const { return this->name; }

const uint64_t Section::getSize() const { return this->size; }

const EA Section::getAddress() const { return this->address; }

bool Section::operator==(const Section& other) const {
  return this->address == other.address && this->size == other.size && this->name == other.name;
}

bool Section::operator!=(const Section& other) const { return !(*this == other); }

void Section::toProtobuf(MessageType* message) const {
  nodeUUIDToBytes(this, *message->mutable_uuid());
  message->set_name(this->name);
  message->set_size(this->size);
  message->set_address(this->address);
}

void Section::fromProtobuf(const MessageType& message) {
  setNodeUUIDFromBytes(this, message.uuid());
  this->name = message.name();
  this->size = message.size();
  this->address = EA(message.address());
}
