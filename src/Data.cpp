#include <gtirb/Data.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Module.hpp>

using namespace gtirb;

std::vector<uint8_t> DataString::getStringBytes(const Module& module) const
{
    return module.getImageByteMap()->getData(this->getEA(), this->size);
}

uint8_t DataRawByte::getByte(const Module& module) const
{
    return module.getImageByteMap()->getData8(this->getEA());
}
