#pragma once

#include <boost/serialization/array.hpp>
#include <boost/serialization/export.hpp>
#include <boost/serialization/map.hpp>
#include <functional>
#include <gsl/gsl>
#include <gtirb/Constants.hpp>
#include <gtirb/EA.hpp>
#include <map>
#include <vector>

namespace gtirb
{
    ///
    /// \class ByteMap
    /// \author John E. Farrier
    ///
    class GTIRB_GTIRB_EXPORT_API ByteMap
    {
    public:
        ///
        /// \class gtirb::ByteMap::Impl
        ///
        /// Implementation details for ByteMap.
        ///
        /// They are exposed here to facilitate unit testing.
        ///
        class Impl
        {
        public:
            ///
            /// Align an address to a page boundary.
            ///
            static EA AddressToAlignedAddress(const EA x);

            /// Give the offset within a page of an address
            ///
            static size_t AddressToOffset(const EA x);

            ///
            /// Number of bytes residing within the first page.
            ///
            static size_t BytesWithinFirstPage(const EA x, const size_t bytes);
        };

        ///
        /// Tests the container for empty.
        ///
        /// Modeled after the STL API.
        ///
        /// \return 	True if the container is empty.
        ///
        bool empty() const;

        ///
        /// The total number of bytes in the image map.
        ///
        /// Modeled after the STL API.
        ///
        size_t size() const;

        ///
        /// Sets data at the given address.
        ///
        /// \param  ea      The address to store the data.
        /// \param  x       The data to store (honoring Endianness).
        ///
        void setData(EA ea, uint8_t x);

        ///
        /// Sets data at the given address.
        ///
        /// \param  ea      The address to store the data.
        /// \param  x       The data to store (honoring Endianness).
        ///
        void setData(EA ea, uint16_t x);

        ///
        /// Sets data at the given address.
        ///
        /// \param  ea      The address to store the data.
        /// \param  x       The data to store (honoring Endianness).
        ///
        void setData(EA ea, uint32_t x);

        ///
        /// Sets data at the given address.
        ///
        /// \param  ea      The address to store the data.
        /// \param  x       The data to store (honoring Endianness).
        ///
        void setData(EA ea, uint64_t x);

        ///
        /// Sets byte map at the given address.
        ///
        /// \param  ea      The address to store the data.
        /// \param  x       A pointer to the data to store (honoring Endianness).
        /// \param  len     The length of the data (the number of bytes) at the pointer.
        ///
        void setData(EA ea, uint8_t* const x, size_t len);

        ///
        /// Get a byte of data at the given address.
        ///
        /// \param  x       The starting address for the data.
        ///
        uint8_t getData8(EA x) const;

        ///
        /// Get a word of data at the given address.
        ///
        /// \param  x       The starting address for the data.
        ///
        uint16_t getData16(EA x) const;

        ///
        /// Get a dword of data at the given address.
        ///
        /// \param  x       The starting address for the data.
        ///
        uint32_t getData32(EA x) const;

        ///
        /// Get a qword of data at the given address.
        ///
        /// \param  x       The starting address for the data.
        ///
        uint64_t getData64(EA x) const;

        ///
        /// Get data at the given address.
        ///
        /// Use the gtirb::utilities functions (i.e. ByteArray8To16) to translate this into 16, 32,
        /// or 64-bits.
        ///
        /// \param  x       The starting address for the data.
        /// \param  bytes   The number of bytes to read.
        ///
        std::vector<uint8_t> getData(EA ea, size_t bytes) const;

        ///
        /// Get data at the given address until a sentinel is found or a limit is reached.
        ///
        /// Use the gtirb::utilities functions (i.e. ByteArray8To16) to translate this into 16, 32,
        /// or 64-bits.
        ///
        /// \param  x       	The starting address for the data.
        /// \param  sentinel   	A byte to stop the 'getData' routine for.
        /// \param  bytes	      The maximum number of bytes to read.
        ///
        std::vector<uint8_t> getDataUntil(EA ea, uint8_t sentinel,
                                          size_t bytes = std::numeric_limits<size_t>::max()) const;

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/)
        {
            ar& data;
        }

    protected:
        ///
        /// \typedef ByteMap::Page
        ///
        /// A page of data within an image's byte map.
        ///
        typedef std::array<uint8_t, gtirb::constants::PageSize> Page;

        ///
        /// Cached lookup of a page.
        ///
        /// Gets the page if it currently exists.
        ///
        /// \return 	A pointer to the page at the address, or nullptr if it does not exist.
        ///
        const ByteMap::Page* const getPage(const EA x) const;

        ///
        /// Adds a new page to be explicitly represented in the map.
        ///
        /// \return 	A pointer to the newly created Page.
        ///
        ByteMap::Page* getOrCreatePage(const EA x);

    private:
        std::map<gtirb::EA, ByteMap::Page> data;
    };
} // namespace gtirb

BOOST_CLASS_EXPORT_KEY(gtirb::ByteMap);
