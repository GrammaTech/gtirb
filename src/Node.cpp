#include <boost/serialization/export.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <gsl/gsl>
#include <gtirb/Node.hpp>
#include <gtirb/Table.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::Node);

std::map<UUID, Node*> Node::uuidMap;

Node* Node::getByUUID(UUID uuid)
{
    auto found = Node::uuidMap.find(uuid);
    if(found != Node::uuidMap.end())
    {
        return found->second;
    }
    else
    {
        return nullptr;
    }
}

// UUID construction is a bottleneck in the creation of Node.  (~0.5ms)
Node::Node() : uuid(boost::uuids::random_generator()())
{
    Node::uuidMap[this->uuid] = this;
}

Node::~Node()
{
    Node::uuidMap.erase(this->uuid);
}

void Node::setUUID()
{
    Node::uuidMap.erase(this->uuid);
    this->uuid = boost::uuids::random_generator()();
    Node::uuidMap[this->uuid] = this;
}

void Node::setUUID(UUID x)
{
    Node::uuidMap.erase(this->uuid);
    this->uuid = x;
    Node::uuidMap[this->uuid] = this;
}

UUID Node::getUUID() const
{
    return this->uuid;
}

void Node::setLocalProperty(std::string name, gtirb::variant value)
{
    this->localProperties[name] = std::move(value);
}

gtirb::variant Node::getLocalProperty(const std::string& x) const
{
    return this->localProperties.at(x);
}

bool Node::removeLocalProperty(const std::string& x)
{
    const auto found = this->localProperties.find(x);

    if(found != std::end(this->localProperties))
    {
        this->localProperties.erase(found);
        return true;
    }

    return false;
}

size_t Node::getLocalPropertySize() const
{
    return this->localProperties.size();
}

bool Node::getLocalPropertyEmpty() const
{
    return this->localProperties.empty();
}

void Node::clearLocalProperties()
{
    this->localProperties.clear();
}

std::map<std::string, gtirb::variant>::iterator Node::beginLocalProperties()
{
    return std::begin(this->localProperties);
}

std::map<std::string, gtirb::variant>::const_iterator Node::beginLocalProperties() const
{
    return std::begin(this->localProperties);
}

std::map<std::string, gtirb::variant>::iterator Node::endLocalProperties()
{
    return std::end(this->localProperties);
}

std::map<std::string, gtirb::variant>::const_iterator Node::endLocalProperties() const
{
    return std::end(this->localProperties);
}

template <class Archive>
void Node::serialize(Archive& ar, const unsigned int /*version*/)
{
    UUID oldId = this->uuid;

    ar & this->localProperties;
    ar & this->uuid.data;

    // When deserializing, update uuidMap
    if(this->uuid != oldId)
    {
        Node::uuidMap.erase(oldId);
        Node::uuidMap[this->uuid] = this;
    }
}
