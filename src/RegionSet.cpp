#include <gtirb/Module.hpp>
#include <gtirb/Region.hpp>
#include <gtirb/RegionSet.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::RegionSet);

Region* RegionSet::getRegion(EA x) const
{
    const auto found = std::find_if(this->contents.begin(), this->contents.end(), [x](auto& r) {
        const auto regionEAs = r->getEAs();
        return std::find(std::begin(regionEAs), std::end(regionEAs), x) != std::end(regionEAs);
    });

    if(found != this->contents.end())
    {
        return found->get();
    }

    return nullptr;
}

Region* RegionSet::getOrCreateRegion(gtirb::EA x)
{
    auto region = this->getRegion(x);

    if(region == nullptr)
    {
        auto newRegion = std::make_unique<Region>();
        newRegion->addEA(x);
        region = newRegion.get();
        this->contents.push_back(std::move(newRegion));
    }

    return region;
}
