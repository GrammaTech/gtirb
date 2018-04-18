#pragma once

#include <cstdint>
#include <gtirb/Export.hpp>
#include <vector>

namespace gtirb
{
    namespace utilities
    {
    	///
    	/// Convert an array (vector) of 8-bit unsigned values to a vector of 16-bit unsigned values.
    	/// 
    	/// The input array must be evenly divisible by two.  If it is not, a std::range_error is thrown.
    	///
    	/// \param x		A reference to the vector of bytes to merge.
    	/// \param swap 	If true, bytes will be swapped as they are merged.
    	///
        GTIRB_GTIRB_EXPORT_API std::vector<uint16_t> ByteArray8To16(const std::vector<uint8_t>& x, bool swap = false);
    }
} // namespace gtirb
