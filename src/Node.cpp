#include <boost/uuid/uuid_generators.hpp>
#include <gtirb/Node.hpp>

using namespace gtirb;

Node::Node() : uuid(boost::uuids::random_generator()())
{
}

Node* Node::getNodeParent() const
{
	return this->nodeParent;
}

void Node::setUUID()
{
    this->uuid = boost::uuids::random_generator()();
}

void Node::setUUID(boost::uuids::uuid x)
{
    this->uuid = x;
}

boost::uuids::uuid Node::getUUID() const
{
    return this->uuid;
}

void Node::addTable(std::string name, std::unique_ptr<gtirb::Table>&& x)
{
	this->tables[std::move(name)] = std::move(x);
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
