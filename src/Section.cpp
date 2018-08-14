#include "Section.hpp"
#include "Serialization.hpp"
#include <proto/Section.pb.h>

using namespace gtirb;

Section::Section(std::string N, EA Ea, uint64_t S)
    : Node(), Name(N), Address(Ea), Size(S) {}

const std::string& Section::getName() const { return this->Name; }

const uint64_t Section::getSize() const { return this->Size; }

const EA Section::getAddress() const { return this->Address; }

bool Section::operator==(const Section& Other) const {
  return this->Address == Other.Address && this->Size == Other.Size &&
         this->Name == Other.Name;
}

bool Section::operator!=(const Section& Other) const {
  return !(*this == Other);
}

void Section::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_name(this->Name);
  Message->set_size(this->Size);
  Message->set_address(this->Address);
}

void Section::fromProtobuf(const MessageType& Message) {
  setNodeUUIDFromBytes(this, Message.uuid());
  this->Name = Message.name();
  this->Size = Message.size();
  this->Address = EA(Message.address());
}
