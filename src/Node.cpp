#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <gsl/gsl>
#include <gtirb/Node.hpp>

using namespace gtirb;

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

Node::Node(const Node& other) : localProperties(other.localProperties)
{
    this->setUUID();
}

Node::Node(Node&& other) noexcept
    : localProperties(std::move(other.localProperties)), uuid(std::move(other.uuid))
{
    other.uuid = UUID();
    Node::uuidMap[this->uuid] = this;
}

Node& Node::operator=(Node&& other) noexcept
{
    // Update UUID map
    auto found = Node::uuidMap.find(other.uuid);
    assert(found != Node::uuidMap.end());
    this->uuid = other.uuid;
    found->second = this;
    other.uuid = UUID();

    this->localProperties = std::move(other.localProperties);
    return *this;
}

Node::~Node() noexcept
{
    auto found = Node::uuidMap.find(this->uuid);
    if(found != Node::uuidMap.end())
    {
        assert(found->second == this);
        Node::uuidMap.erase(found);
    }
}

void Node::setUUID()
{
    Node::uuidMap.erase(this->uuid);
    this->uuid = boost::uuids::random_generator()();
    Node::uuidMap[this->uuid] = this;
}

void Node::setUUID(UUID x)
{
    // UUID should not previously exist
    assert(Node::uuidMap.find(x) == Node::uuidMap.end());

    Node::uuidMap.erase(this->uuid);
    this->uuid = x;
    Node::uuidMap.emplace(x, this);
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
