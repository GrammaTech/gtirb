#include <boost/lexical_cast.hpp>
#include <boost/serialization/export.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <gsl/gsl>
#include <gtirb/Node.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <gtirb/NodeValidators.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::Node);

// UUID construction is a bottleneck in the creation of Node.  (~0.5ms)
Node::Node() : uuid(boost::lexical_cast<std::string>(boost::uuids::random_generator()()))
{
    this->addParentValidator([](const Node* const node, const Node* const parent) {
        // We should not become a parent to ourself.
        if((parent != nullptr) && (parent->getUUID() != node->getUUID()))
        {
            // Search all the way up just to make sure there isn't a circular reference.
            if(node->getNodeParent() != nullptr)
            {
                return node->getNodeParent()->getIsValidParent(parent);
            }

            // We are the root and all is still valid.
            return true;
        }

        // Circular reference detected.
        return false;
    });
}

Node* Node::getNodeParent() const
{
    return this->nodeParent;
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

bool Node::getIsValidParent(const Node* const x) const
{
    for(const auto& i : this->parentValidators)
    {
        if(!i(this, x))
        {
            return false;
        }
    }

    return true;
}

void Node::push_back(std::unique_ptr<gtirb::Node>&& x)
{
    Expects(x != nullptr);
    Expects(x->getNodeParent() == nullptr);

    if(x->getIsValidParent(this) == true)
    {
        x->nodeParent = this;
        this->children.push_back(std::move(x));
    }
    else
    {
        throw gtirb::NodeStructureError("Invalid parent/child relationship.", __FILE__, __LINE__);
    }
}

bool Node::empty() const
{
    return this->children.empty();
}

size_t Node::size() const
{
    return this->children.size();
}

void Node::clear()
{
    this->children.clear();
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

    if(this->nodeParent != nullptr)
    {
        return this->nodeParent->getTable(x);
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

gtirb::Node* Node::at(size_t x)
{
    return this->children.at(x).get();
}

const gtirb::Node* const Node::at(size_t x) const
{
    return this->children.at(x).get();
}

Node::iterator Node::begin()
{
    return Node::iterator(std::begin(this->children));
}

Node::iterator Node::end()
{
    return Node::iterator(std::end(this->children));
}

Node::const_iterator Node::begin() const
{
    return Node::const_iterator(std::begin(this->children));
}

Node::const_iterator Node::end() const
{
    return Node::const_iterator(std::end(this->children));
}

void Node::addParentValidator(std::function<bool(const Node* const, const Node* const)> x)
{
    this->parentValidators.push_back(x);
}
