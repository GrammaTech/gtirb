#include <gtirb/Region.hpp>
#include <gtirb/RegionSet.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::Region);

void Region::addEA(gtirb::EA x)
{
    this->eas.insert(x);
}

std::set<gtirb::EA> Region::getEAs() const
{
    return this->eas;
}
