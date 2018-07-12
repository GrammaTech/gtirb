#include <proto/Region.pb.h>
#include <gtirb/Region.hpp>
#include <gtirb/RegionSet.hpp>
#include <gtirb/Serialization.hpp>

using namespace gtirb;

std::set<gtirb::EA>& Region::getEAs()
{
    return this->eas;
}

const std::set<gtirb::EA>& Region::getEAs() const
{
    return this->eas;
}

void Region::toProtobuf(MessageType* message) const
{
    nodeUUIDToBytes(this, *message->mutable_uuid());
    containerToProtobuf(this->eas, message->mutable_eas());
}

void Region::fromProtobuf(const MessageType& message)
{
    setNodeUUIDFromBytes(this, message.uuid());
    containerFromProtobuf(this->eas, message.eas());
}
