#include <gsl/gsl>
#include <gtirb/CFG.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Utilities.hpp>
#include <iostream>

using namespace gtirb;

std::vector<uint16_t> gtirb::utilities::ByteArray8To16(const std::vector<uint8_t>& x, bool swap)
{
    std::vector<uint16_t> vec;

    union Conversion {
        uint8_t Bytes[sizeof(uint16_t)];
        uint16_t Word;
    };

    if(x.size() % sizeof(uint16_t) != 0)
    {
        throw std::range_error(
            "The input vector held an incorrect number of bytes and could not be converted.");
    }

    for(int i = 0; i < static_cast<int>(x.size()); i += static_cast<int>(sizeof(uint16_t)))
    {
        Conversion cvt{0};

        for(size_t j = 0; j < sizeof(uint16_t); ++j)
        {
            cvt.Bytes[j] = x[i + j];
        }

        if(swap == true)
        {
            cvt.Word = gtirb::utilities::SwapEndian(cvt.Word);
        }

        vec.push_back(cvt.Word);
    }

    return vec;
}

std::vector<uint32_t> gtirb::utilities::ByteArray8To32(const std::vector<uint8_t>& x, bool swap)
{
    std::vector<uint32_t> vec;

    union Conversion {
        uint8_t Bytes[sizeof(uint32_t)];
        uint32_t Word;
    };

    if(x.size() % sizeof(uint32_t) != 0)
    {
        throw std::range_error(
            "The input vector held an incorrect number of bytes and could not be converted.");
    }

    for(int i = 0; i < static_cast<int>(x.size()); i += static_cast<int>(sizeof(uint32_t)))
    {
        Conversion cvt{0};

        for(size_t j = 0; j < sizeof(uint32_t); ++j)
        {
            cvt.Bytes[j] = x[i + j];
        }

        if(swap == true)
        {
            cvt.Word = gtirb::utilities::SwapEndian(cvt.Word);
        }

        vec.push_back(cvt.Word);
    }

    return vec;
}

std::vector<uint64_t> gtirb::utilities::ByteArray8To64(const std::vector<uint8_t>& x, bool swap)
{
    std::vector<uint64_t> vec;

    union Conversion {
        uint8_t Bytes[sizeof(uint64_t)];
        uint64_t Word;
    };

    if(x.size() % sizeof(uint64_t) != 0)
    {
        throw std::range_error(
            "The input vector held an incorrect number of bytes and could not be converted.");
    }

    for(int i = 0; i < static_cast<int>(x.size()); i += static_cast<int>(sizeof(uint64_t)))
    {
        Conversion cvt{0};

        for(size_t j = 0; j < sizeof(uint64_t); ++j)
        {
            cvt.Bytes[j] = x[i + j];
        }

        if(swap == true)
        {
            cvt.Word = gtirb::utilities::SwapEndian(cvt.Word);
        }

        vec.push_back(cvt.Word);
    }

    return vec;
}
