#pragma once

#include <boost/variant.hpp>
#include <gtirb/EA.hpp>

namespace gtirb
{
    ///
    /// \typedef gtirb::variant
    ///
    /// This ensures that the same variant is used throughout GT-IRB.
    ///
    typedef boost::variant<uint8_t, uint16_t, uint32_t, uint64_t, int8_t, int16_t, int32_t, int64_t,
                           double, std::string, gtirb::EA>
        variant;
} // namespace gtirb
