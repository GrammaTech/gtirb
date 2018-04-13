#include <gtirb/Utilities.hpp>
#include <iostream>

std::vector<uint16_t> gtirb::util::ByteArray8To16(const std::vector<uint8_t>& x, bool swap)
{
    union Conversion {
        uint8_t Bytes[2];
        uint16_t Word;
    };

    if(x.size() % 2 != 0)
    {
        throw std::range_error("The input vector was an odd number of bytes (uint8_t) and could not be converted to uint16_t.");
    }

    // Prevent branching during conversion.
    int first = 0;
    int second = 1;

    if(swap == true)
    {
    	first = 1;
    	second = 0;
    }

    std::vector<uint16_t> vec;

    for(size_t i = 0; i < x.size(); i += 2)
    {
        Conversion cvt;

       	cvt.Bytes[first] = x[i];
       	cvt.Bytes[second] = x[i + 1];

        vec.push_back(cvt.Word);
    }

    return vec;
}
