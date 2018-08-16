#include "Section.hpp"
#include "Serialization.hpp"
#include <proto/Section.pb.h>

using namespace gtirb;

const std::string& Section::getName() const { return this->Name; }

uint64_t Section::getSize() const { return this->Size; }

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

Section* Section::fromProtobuf(Context& C, const MessageType& Message) {
  auto* S =
      Section::Create(C, Message.name(), EA(Message.address()), Message.size());
  setNodeUUIDFromBytes(S, Message.uuid());
  return S;
}
