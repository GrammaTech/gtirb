#include <proto/Data.pb.h>
#include <boost/serialization/export.hpp>
#include <gtirb/Data.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Serialization.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::Data);

template <class Archive>
void Data::serialize(Archive& ar, const unsigned int /*version*/)
{
    ar& boost::serialization::base_object<Node>(*this);
    ar & this->ea;
    ar & this->size;
}
EA Data::getEA() const
{
    return this->ea;
}

uint64_t Data::getSize() const
{
    return this->size;
}

std::vector<uint8_t> Data::getBytes(const Module& module) const
{
    return module.getImageByteMap().getData(this->getEA(), this->getSize());
}

void Data::toProtobuf(MessageType* message) const
{
    nodeUUIDToBytes(this, *message->mutable_uuid());
    message->set_ea(this->ea);
    message->set_size(this->size);
}

void Data::fromProtobuf(const MessageType& message)
{
    setNodeUUIDFromBytes(this, message.uuid());
    this->ea = EA(message.ea());
    this->size = message.size();
}
