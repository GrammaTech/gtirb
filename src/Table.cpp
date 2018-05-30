#include <boost/serialization/map.hpp>
#include <boost/serialization/string.hpp>
#include <boost/serialization/variant.hpp>
#include <boost/serialization/vector.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/Table.hpp>

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::Table);

using namespace gtirb;

Table::~Table() = default;

size_t Table::size() const
{
    return contents.size();
}

void Table::clear()
{
    contents.clear();
}

template <class Archive>
void Table::serialize(Archive& ar, const unsigned int /*version*/)
{
    ar & this->contents;
}
