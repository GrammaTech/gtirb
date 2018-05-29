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
    boost::serialization::void_cast_register<SectionTable, Table>();
    ar & this->data;
}

void SectionTable::serialize(boost::archive::polymorphic_oarchive& ar,
                             const unsigned int version) const
{
    boost::serialization::void_cast_register<SectionTable, Table>();
    ar & this->data;
}

SectionTable::const_iterator SectionTable::begin() const noexcept
{
    return std::begin(this->data);
}

SectionTable::const_iterator SectionTable::end() const noexcept
{
    return std::end(this->data);
}

SectionTable::const_iterator SectionTable::find(const EA& startAddress) const
{
    return this->data.find(startAddress);
}

void SectionTable::addSection(Section s)
{
    this->data[s.startingAddress] = s;
}
