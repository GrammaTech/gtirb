#pragma once

#include <cstdint>
#include <vector>

namespace gtirb
{
    namespace util
    {
        std::vector<uint16_t> ByteArray8To16(const std::vector<uint8_t>& x, bool swap = false);
    }
}
