#include <gtirb/Region.hpp>
#include <gtirb/RegionSet.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::Region);

std::set<gtirb::EA>& Region::getEAs()
{
    return this->eas;
}

const std::set<gtirb::EA>& Region::getEAs() const
{
    return this->eas;
}
