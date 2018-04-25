#pragma once

#include <array>
#include <boost/filesystem.hpp>
#include <gsl/gsl>
#include <gtirb/ByteMap.hpp>
#include <gtirb/Constants.hpp>
#include <gtirb/EA.hpp>
#include <gtirb/FilesystemSerialization.hpp>
#include <gtirb/Node.hpp>
#include <set>

namespace gtirb
{
    ///
    /// \class ImageByteMap
    /// \author John E. Farrier
    ///
    /// Contains the loaded raw image data for the module (binary).
    ///
    class GTIRB_GTIRB_EXPORT_API ImageByteMap : public Node
    {
    public:
        ///
        /// \enum gtirb::ImageByteMap::LFCMTypeMask
        ///
        /// Bit masks to pack pointer size, memory model, and calling convention into 8 bytes.
        ///
        enum LFCMTypeMasks
        {
            CM_MASK = 0x03,      /// The mask for size of pointers.
            CM_UNKNOWN = 0x00,   /// Unknown pointer size.
            CM_N8_F16 = 0x01,    /// 1: near 1byte, far 2bytes.
            CM_N16_F32 = 0x02,   /// 2: near 2bytes, far 4bytes
            CM_N32_F48 = 0x03,   /// 4: near 4bytes, far 6bytes
            MM_MASK = 0x0C,      /// The mask for the memory model.
            MM_NN = 0x00,        /// small:   code=near, data=near (or unknown if CM_UNKNOWN)
            MM_FF = 0x04,        /// large:   code=far, data=far
            MM_NF = 0x08,        /// compact: code=near, data=far
            MM_FN = 0x0C,        /// medium:  code=far, data=near
            CC_MASK = 0xF0,      /// A mask for just calling conventions.
            CC_INVALID = 0x00,   /// this value is invalid
            CC_UNKNOWN = 0x10,   /// unknown calling convention
            CC_VOIDARG = 0x20,   /// function without arguments
            CC_CDECL = 0x30,     /// stack
            CC_ELLIPSIS = 0x40,  /// cdecl + ellipsis
            CC_STDCALL = 0x50,   /// stack, purged
            CC_PASCAL = 0x60,    /// stack, purged, reverse order of args
            CC_FASTCALL = 0x70,  /// stack, first args are in regs (compiler-dependent)
            CC_THISCALL = 0x80,  /// stack, first arg is in reg (compiler-dependent)
            CC_MANUAL = 0x90,    /// special case for compiler specific
            CC_RESERVED5 = 0xA0, /// reserved
            CC_RESERVED4 = 0xB0, /// reserved
            CC_RESERVED3 = 0xC0, /// reserved
            CC_RESERVED2 = 0xD0, /// reserved
            CC_RESERVED1 = 0xE0, /// reserved
            CC_SPECIAL = 0xF0    /// locations of all arguments and the return
        };

        ///
        /// \enum gtirb::ImageByteMap::ContentSource
        ///
        enum class ContentSource : uint8_t
        {
            Unknown,
            Exe,
            IDAFull,
            IDAPartial
        };

        ImageByteMap();

        virtual ~ImageByteMap() = default;

        ///
        /// \return     Sets the file name of the image.
        ///
        void setFileName(boost::filesystem::path x);

        ///
        /// \return     The loaded file name and path.
        ///
        boost::filesystem::path getFileName() const;

        ///
        /// Sets the base addrress of loaded file.
        ///
        void setBaseAddress(EA x);

        ///
        /// Gets the base addrress of loaded file.
        ///
        EA getBaseAddress() const;

        ///
        /// Sets the entry point of loaded file.
        ///
        void setEntryPointAddress(EA x);

        ///
        /// Gets the entry point of loaded file.
        ///
        EA getEntryPointAddress() const;

        ///
        /// If an invalid pair is passed in, the min and max will be set to an invalid state
        /// (gtirb::constants::BadAddress).  The range's min and max values are inclusive.
        ///
        /// \param      x   The minimum and maximum effective address (EA) for this Module.
        /// \return     False if the pair's first is > the pair's second.
        ///
        bool setEAMinMax(std::pair<gtirb::EA, gtirb::EA> x);

        ///
        /// Gets the minimum and maximum effective address (EA) for this Module.
        ///
        /// Check return values for gtirb::constants::BadAddress.
        ///
        /// \return     The minimum and maximum effective address (EA) for this Module.
        ///
        std::pair<gtirb::EA, gtirb::EA> getEAMinMax() const;

        ///
        ///
        ///
        void setRebaseDelta(int64_t x);

        ///
        ///
        ///
        int64_t getRebaseDelta() const;

        ///
        /// Sets the memory model and calling convention word from the loaded file.
        ///
        /// Use the LFCMTypeMasks.
        ///
        void setLFCM(uint8_t x);

        ///
        /// Gets the memory model and calling convention word from the loaded file.
        ///
        /// Use LFCMTypeMasks to decode.
        ///
        uint8_t getLFCM() const;

        ///
        /// Marks the loaded image as having been relocated.
        ///
        /// This is primarily useful for loaders that load from sources that provide
        /// already-relocated content, such as IDA.
        ///
        void setIsRelocated();

        ///
        /// \return     True if the loaded image has been relocated.
        ///
        bool getIsRelocated() const;

        ///
        /// Set the Global Offset Table EA.
        ///
        /// This is used for relocations expressed with "GOT".  It appears that it is the address of
        /// the symbol _GLOBAL_OFFSET_TABLE_.
        ///
        void setGlobalOffsetTableAddress(EA x);

        ///
        /// This is used for relocations expressed with "GOT".  It appears that it is the address of
        /// the symbol _GLOBAL_OFFSET_TABLE_.
        ///
        EA getGlobalOffsetTableAddress() const;

        ///
        ///
        ///
        void setContentSource(ContentSource x);

        ///
        ///
        ///
        ContentSource getContentSource() const;

        ///
        /// Tests the byte map for empty.
        ///
        /// Modeled after the STL API.
        ///
        /// \return     True if the byte map is empty.
        ///
        /// \sa gtirb::ByteMap
        ///
        bool getDataEmpty() const;

        ///
        /// The total number of bytes in the image byte map.
        ///
        /// Modeled after the STL API.
        ///
        /// \sa gtirb::ByteMap
        ///
        size_t getDataSize() const;

        ///
        /// Sets byte map at the given address.
        ///
        /// The given address must be within the minimum and maximum EA.
        ///
        /// \throws std::out_of_range   Throws if the address to set data at is outside of the
        /// minimum and maximum EA.
        ///
        /// \param  ea      The address to store the data.
        /// \param  x       The data to store (honoring Endianness).
        ///
        /// \sa gtirb::ByteMap
        ///
        void setData(EA ea, uint8_t x);

        ///
        /// Sets byte map at the given address.
        ///
        /// The given address must be within the minimum and maximum EA.
        ///
        /// \throws std::out_of_range   Throws if the address to set data at is outside of the
        /// minimum and maximum EA.
        ///
        /// \param  ea      The address to store the data.
        /// \param  x       The data to store (honoring Endianness).
        ///
        /// \sa gtirb::ByteMap
        ///
        void setData(EA ea, uint16_t x);

        ///
        /// Sets byte map at the given address.
        ///
        /// The given address must be within the minimum and maximum EA.
        ///
        /// \throws std::out_of_range   Throws if the address to set data at is outside of the
        /// minimum and maximum EA.
        ///
        /// \param  ea      The address to store the data.
        /// \param  x       The data to store (honoring Endianness).
        ///
        /// \sa gtirb::ByteMap
        ///
        void setData(EA ea, uint32_t x);

        ///
        /// Sets byte map at the given address.
        ///
        /// The given address must be within the minimum and maximum EA.
        ///
        /// \throws std::out_of_range   Throws if the address to set data at is outside of the
        /// minimum and maximum EA.
        ///
        /// \param  ea      The address to store the data.
        /// \param  x       The data to store (honoring Endianness).
        ///
        /// \sa gtirb::ByteMap
        ///
        void setData(EA ea, uint64_t x);

        ///
        /// Sets byte map at the given address.
        ///
        /// The given address must be within the minimum and maximum EA.
        ///
        /// \throws std::out_of_range   Throws if the address to set data at is outside of the
        /// minimum and maximum EA.
        ///
        /// \param  ea      The address to store the data.
        /// \param  x       A pointer to the data to store (honoring Endianness).
        /// \param  bytes   The length of the data at the pointer.
        ///
        /// \sa gtirb::ByteMap
        ///
        void setData(EA ea, uint8_t* const x, size_t bytes);

        ///
        /// Get a byte of data from the byte map at the given address.
        ///
        /// \param  x       The starting address for the data.
        ///
        /// \sa gtirb::ByteMap
        ///
        uint8_t getData8(EA x) const;

        ///
        /// Get a word of data from the byte map  at the given address.
        ///
        /// \param  x       The starting address for the data.
        ///
        /// \sa gtirb::ByteMap
        ///
        uint16_t getData16(EA x) const;

        ///
        /// Get a dword of data from the byte map  at the given address.
        ///
        /// \param  x       The starting address for the data.
        ///
        /// \sa gtirb::ByteMap
        ///
        uint32_t getData32(EA x) const;

        ///
        /// Get a qword of data from the byte map  at the given address.
        ///
        /// \param  x       The starting address for the data.
        ///
        /// \sa gtirb::ByteMap
        ///
        uint64_t getData64(EA x) const;

        ///
        /// Get data from the byte map  at the given address.
        ///
        /// Use the gtirb::utilities functions (i.e. ByteArray8To16) to translate this into 16, 32,
        /// or 64-bits.
        ///
        /// \param  x       The starting address for the data.
        /// \param  bytes   The number of bytes to read.
        ///
        /// \sa gtirb::ByteMap
        ///
        std::vector<uint8_t> getData(EA ea, size_t bytes) const;

        ///
        /// Get data from the byte map  at the given address until a sentinel is found or a limit is
        /// reached.
        ///
        /// Use the gtirb::utilities functions (i.e. ByteArray8To16) to translate this into 16, 32,
        /// or 64-bits.
        ///
        /// \param  x           The starting address for the data.
        /// \param  sentinel    A byte to stop the 'getData' routine for.
        /// \param  bytes         The maximum number of bytes to read.
        ///
        /// \sa gtirb::ByteMap
        ///
        std::vector<uint8_t> getDataUntil(EA ea, uint8_t sentinel,
                                          size_t bytes = std::numeric_limits<size_t>::max()) const;

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int)
        {
            ar& boost::serialization::base_object<Node>(*this);
            ar & this->byteMap;
            GTIRB_SERIALIZE_FILESYSTEM_PATH(ar, this->fileName);
            ar & this->eaMinMax;
            ar & this->baseAddress;
            ar & this->entryPointAddress;
            ar & this->globalOffsetTableAddress;
            ar & this->rebaseDelta;
            ar & this->lfcm;
            ar & this->contentSource;
            ar & this->isRelocated;
        }

    private:
        // Storage for the entire contents of the loaded image.
        gtirb::ByteMap byteMap;
        boost::filesystem::path fileName;
        std::pair<gtirb::EA, gtirb::EA> eaMinMax{};
        EA baseAddress{};
        EA entryPointAddress{};
        EA globalOffsetTableAddress{};
        int64_t rebaseDelta{0};
        uint8_t lfcm{0x00};
        ContentSource contentSource{};
        bool isRelocated{false};
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::ImageByteMap);
