#include <gtirb/Module.hpp>
#include <gtirb/NodeValidators.hpp>
#include <gtirb/Region.hpp>
#include <gtirb/RegionSet.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::RegionSet);

RegionSet::RegionSet() : Node()
{
    this->addParentValidator(gtirb::NodeValidatorHasParentOfType<gtirb::Module>());
    this->addParentValidator(NodeValidatorHasNoSiblingsOfType<gtirb::RegionSet>());
}

Region* RegionSet::getRegion(gtirb::EA x) const
{
    const auto regions = GetChildrenOfType<Region>(this);
    const auto found = std::find_if(std::begin(regions), std::end(regions), [x](Region* r) {
        const auto regionEAs = r->getEAs();
        return std::find(std::begin(regionEAs), std::end(regionEAs), x) != std::end(regionEAs);
    });

    if(found != std::end(regions))
    {
        return *found;
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
        this->push_back(std::move(newRegion));
    }

    return region;
}
