#include <boost/serialization/export.hpp>
#include <boost/serialization/string.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolicOperand.hpp>

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::SymStackConst);
BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::SymAddrConst);
BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::SymAddrAddr);

using namespace gtirb;

template <class Archive>
void SymStackConst::serialize(Archive& ar, const unsigned int /*version*/)
{
    ar & this->negate;
    ar & this->offset;
    ar & this->displacement;
    ar & this->symbol;
}

template <class Archive>
void SymAddrConst::serialize(Archive& ar, const unsigned int /*version*/)
{
    ar & this->displacement;
    ar & this->symbol;
}

template <class Archive>
void SymAddrAddr::serialize(Archive& ar, const unsigned int /*version*/)
{
    ar & this->scale;
    ar & this->offset;
    ar & this->symbol1;
    ar & this->symbol2;
}
