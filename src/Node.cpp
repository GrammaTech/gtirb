#include <boost/lexical_cast.hpp>
#include <boost/serialization/export.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <gsl/gsl>
#include <gtirb/Node.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::Node);

// UUID construction is a bottleneck in the creation of Node.  (~0.5ms)
Node::Node() : uuid(boost::lexical_cast<std::string>(boost::uuids::random_generator()()))
{
}

void Node::setUUID()
{
    this->uuid = boost::lexical_cast<std::string>(boost::uuids::random_generator()());
}

void Node::setUUID(std::string x)
{
    this->uuid = x;
}

std::string Node::getUUID() const
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

Table* Node::addTable(std::string name, std::unique_ptr<gtirb::Table>&& x)
{
    return (this->tables[std::move(name)] = std::move(x)).get();
}

gtirb::Table* const Node::getTable(const std::string& x) const
{
    const auto found = this->tables.find(x);
    if(found != std::end(this->tables))
    {
        return (*found).second.get();
    }

    return nullptr;
}

bool Node::removeTable(const std::string& x)
{
    const auto found = this->tables.find(x);

    if(found != std::end(this->tables))
    {
        this->tables.erase(found);
        return true;
    }

    return false;
}

size_t Node::getTableSize() const
{
    return this->tables.size();
}

bool Node::getTablesEmpty() const
{
    return this->tables.empty();
}

void Node::clearTables()
{
    this->tables.clear();
}
