#include <gtirb/Node.hpp>
#include <boost/uuid/uuid_generators.hpp>

using namespace gtirb;

Node::Node() : uuid(boost::uuids::random_generator()())
{

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

void Node::setLocalProperty(std::string name, gtirb::any value)
{
	this->localProperties[name] = std::move(value);
}

gtirb::any Node::getLocalProperty(const std::string& x) const
{
	return this->localProperties.at(x);
}

bool Node::removeLocalProperty(const std::string& x)
{
	auto found = this->localProperties.find(x);
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
