#include <boost/serialization/export.hpp>
#include <gtirb/Data.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Module.hpp>

using namespace gtirb;

BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::Data);
BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::DataLabelMarker);
BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::DataPLTReference);
BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::DataPointer);
BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::DataPointerDiff);
BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::DataString);
BOOST_CLASS_EXPORT_IMPLEMENT(gtirb::DataRawByte);

std::vector<uint8_t> DataString::getStringBytes(const Module& module) const
{
    return module.getImageByteMap()->getData(this->getEA(), this->size);
}

uint8_t DataRawByte::getByte(const Module& module) const
{
    return module.getImageByteMap()->getData8(this->getEA());
}

template <class Archive>
void Data::serialize(Archive& ar, const unsigned int /*version*/)
{
    ar& boost::serialization::base_object<Node>(*this);
    ar & this->ea;
}

template <class Archive>
void DataPLTReference::serialize(Archive& ar, const unsigned int /*version*/)
{
    ar& boost::serialization::base_object<Data>(*this);
    ar & this->function;
}

template <class Archive>
void DataPointer::serialize(Archive& ar, const unsigned int /*version*/)
{
    ar& boost::serialization::base_object<Data>(*this);
    ar & this->content;
}

template <class Archive>
void DataPointerDiff::serialize(Archive& ar, const unsigned int /*version*/)
{
    ar& boost::serialization::base_object<Data>(*this);
    ar & this->symbol1;
    ar & this->symbol2;
}

template <class Archive>
void DataString::serialize(Archive& ar, const unsigned int /*version*/)
{
    ar& boost::serialization::base_object<Data>(*this);
    ar & this->size;
}
