#pragma once

#include <cstdint>
#include <gtirb/Export.hpp>
#include <vector>

namespace gtirb
{
    namespace util
    {
        GTIRB_GTIRB_EXPORT_API std::vector<uint16_t> ByteArray8To16(const std::vector<uint8_t>& x, bool swap = false);
    }
} // namespace gtirb
