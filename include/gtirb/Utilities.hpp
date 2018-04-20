#pragma once

#include <cstdint>
#include <gtirb/Export.hpp>
#include <vector>

namespace gtirb
{
    namespace utilities
    {
        ///
        /// Convert an array (vector) of 8-bit unsigned values to a vector of 16-bit unsigned
        /// values.
        ///
        /// The input array must be evenly divisible by 2.  If it is not, a std::range_error is
        /// thrown.
        ///
        /// \param x		A reference to the vector of bytes to merge.
        /// \param swap 	If true, bytes will be swapped as they are merged.
        ///
        GTIRB_GTIRB_EXPORT_API std::vector<uint16_t> ByteArray8To16(const std::vector<uint8_t>& x,
                                                                    bool swap = false);

        ///
        /// Convert an array (vector) of 8-bit unsigned values to a vector of 32-bit unsigned
        /// values.
        ///
        /// The input array must be evenly divisible by 4.  If it is not, a std::range_error is
        /// thrown.
        ///
        /// \param x        A reference to the vector of bytes to merge.
        /// \param swap     If true, bytes will be swapped as they are merged.
        ///
        GTIRB_GTIRB_EXPORT_API std::vector<uint32_t> ByteArray8To32(const std::vector<uint8_t>& x,
                                                                    bool swap = false);

        ///
        /// Convert an array (vector) of 8-bit unsigned values to a vector of 64-bit unsigned
        /// values.
        ///
        /// The input array must be evenly divisible by 8.  If it is not, a std::range_error is
        /// thrown.
        ///
        /// \param x        A reference to the vector of bytes to merge.
        /// \param swap     If true, bytes will be swapped as they are merged.
        ///
        GTIRB_GTIRB_EXPORT_API std::vector<uint64_t> ByteArray8To64(const std::vector<uint8_t>& x,
                                                                    bool swap = false);

        ///
        /// Swap the Endianness of a POD type.
        ///
        /// This is most useful for use in other templated methods.  Prefer to use a
        /// platform-specific implementation when the type is known.
        ///
        /// \sa
        /// https://stackoverflow.com/questions/105252/how-do-i-convert-between-big-endian-and-little-endian-values-in-c
        ///
        template <typename T>
        T SwapEndian(T u)
        {
            union {
                T u;
                uint8_t u8[sizeof(T)];
            } source{0}, dest{0};

            source.u = u;

            for(int k = 0; k < sizeof(T); k++)
            {
                dest.u8[k] = source.u8[sizeof(T) - k - 1];
            }

            return dest.u;
        }
    } // namespace utilities
} // namespace gtirb
