#include <gtirb/Module.hpp>
#include <gtirb/Region.hpp>
#include <gtirb/RegionSet.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::RegionSet);

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
    newRegion.addEA(x);
    this->contents.push_back(std::move(newRegion));

    return this->contents.back();
}
