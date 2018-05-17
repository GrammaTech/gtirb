#include <boost/serialization/map.hpp>
#include <gtirb/SectionTable.hpp>

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::SectionTable);

using namespace gtirb;

size_t SectionTable::size() const
{
    return data.size();
}

void SectionTable::clear()
{
    data.clear();
}

void SectionTable::serialize(boost::archive::polymorphic_iarchive& ar, const unsigned int version)
{
    ar & this->data;
}

void SectionTable::serialize(boost::archive::polymorphic_oarchive& ar,
                             const unsigned int version) const
{
    ar & this->data;
}
