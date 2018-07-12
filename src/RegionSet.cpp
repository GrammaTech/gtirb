#include <proto/RegionSet.pb.h>
#include <gtirb/Module.hpp>
#include <gtirb/Region.hpp>
#include <gtirb/RegionSet.hpp>
#include <gtirb/Serialization.hpp>

using namespace gtirb;

const Region* RegionSet::getRegion(EA x) const
{
    const auto found =
        std::find_if(this->contents.begin(), this->contents.end(), [x](const auto& r) {
            const auto regionEAs = r.getEAs();
            return std::find(std::begin(regionEAs), std::end(regionEAs), x) != std::end(regionEAs);
        });

    if(found != this->contents.end())
    {
        return &*found;
    }

    return nullptr;
}

Region& RegionSet::createRegion(gtirb::EA x)
{
    Expects(this->getRegion(x) == nullptr);

    Region newRegion;
    newRegion.getEAs().insert(x);
    this->contents.push_back(std::move(newRegion));

    return this->contents.back();
}

void RegionSet::toProtobuf(MessageType* message) const
{
    nodeUUIDToBytes(this, *message->mutable_uuid());
    containerToProtobuf(this->contents, message->mutable_contents());
}

void RegionSet::fromProtobuf(const MessageType& message)
{
    setNodeUUIDFromBytes(this, message.uuid());
    containerFromProtobuf(this->contents, message.contents());
}
