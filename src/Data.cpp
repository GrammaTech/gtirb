#include <boost/serialization/export.hpp>
#include <gtirb/Data.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Module.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::Data);

template <class Archive>
void Data::serialize(Archive& ar, const unsigned int /*version*/)
{
    ar& boost::serialization::base_object<Node>(*this);
    ar & this->ea;
    ar & this->size;
}
EA Data::getEA() const
{
    return this->ea;
}

uint64_t Data::getSize() const
{
    return this->size;
}

std::vector<uint8_t> Data::getBytes(const Module& module) const
{
    return module.getImageByteMap()->getData(this->getEA(), this->getSize());
}
