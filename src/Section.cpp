#include <proto/Section.pb.h>
#include <gtirb/Section.hpp>
#include <gtirb/Serialization.hpp>

using namespace gtirb;

Section::Section(std::string n, uint64_t s, EA ea) : Node(), name(n), size(s), startingAddress(ea)
{
}

const std::string& Section::getName() const
{
    return this->name;
}

const uint64_t Section::getSize() const
{
    return this->size;
}

const EA Section::getStartingAddress() const
{
    return this->startingAddress;
}

EA Section::addressLimit() const
{
    // If base address is bad, return a bad EA.
    if(this->startingAddress == EA{})
    {
        return EA{};
    }
    else
    {
        return EA(this->startingAddress.get() + this->size);
    }
}

bool Section::contains(EA ea) const
{
    return (ea >= this->startingAddress) && (ea < this->addressLimit());
}

bool Section::operator==(const Section& other) const
{
    return this->startingAddress == other.startingAddress && this->size == other.size
           && this->name == other.name;
}

bool Section::operator!=(const Section& other) const
{
    return !(*this == other);
}

void Section::toProtobuf(MessageType* message) const
{
    nodeUUIDToBytes(this, *message->mutable_uuid());
    message->set_name(this->name);
    message->set_size(this->size);
    message->set_starting_address(this->startingAddress);
}

void Section::fromProtobuf(const MessageType& message)
{
    setNodeUUIDFromBytes(this, message.uuid());
    this->name = message.name();
    this->size = message.size();
    this->startingAddress = EA(message.starting_address());
}
