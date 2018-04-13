#include <boost/uuid/uuid_generators.hpp>
#include <gtirb/LocalProperties.hpp>

using namespace gtirb;

void LocalProperties::setLocalProperty(std::string name, gtirb::any value)
{
    this->localProperties[name] = std::move(value);
}

gtirb::any LocalProperties::getLocalProperty(const std::string& x) const
{
    return this->localProperties.at(x);
}

bool LocalProperties::removeLocalProperty(const std::string& x)
{
    const auto found = this->localProperties.find(x);
    
    if(found != std::end(this->localProperties))
    {
        this->localProperties.erase(found);
        return true;
    }

    return false;
}

size_t LocalProperties::getLocalPropertySize() const
{
    return this->localProperties.size();
}

bool LocalProperties::getLocalPropertyEmpty() const
{
    return this->localProperties.empty();
}

void LocalProperties::clearLocalProperties()
{
    this->localProperties.clear();
}

std::map<std::string, gtirb::any>::iterator LocalProperties::beginLocalProperties()
{
    return std::begin(this->localProperties);
}

std::map<std::string, gtirb::any>::const_iterator LocalProperties::beginLocalProperties() const
{
    return std::begin(this->localProperties);
}

std::map<std::string, gtirb::any>::iterator LocalProperties::endLocalProperties()
{
    return std::end(this->localProperties);
}

std::map<std::string, gtirb::any>::const_iterator LocalProperties::endLocalProperties() const
{
    return std::end(this->localProperties);
}
