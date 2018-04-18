#pragma once

#include <array>
#include <boost/filesystem.hpp>
#include <gsl.hpp>
#include <gtirb/Constants.hpp>
#include <gtirb/EA.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Node.hpp>
#include <set>

namespace gtirb
{
    ///
    /// \class LoadedFileMap
    /// \author John E. Farrier
    ///
    /// Contains the raw file data for the module (binary)
    ///
    class GTIRB_GTIRB_EXPORT_API LoadedFileMap : public Node
    {
    public:
        ///
        /// \enum LFCMTypeMask
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

        enum class ContentSource : uint8_t
        {
            Unknown,
            Exe,
            IDAFull,
            IDAPartial
        };

        LoadedFileMap();
        virtual ~LoadedFileMap() = default;

        ///
        /// \return     The number of bytes loaded.
        ///
        size_t load(boost::filesystem::path x);

        ///
        /// \return     The loaded file name and path.
        ///
        boost::filesystem::path getFileName() const;

        ///
        /// \return     The file size, in bytes.
        ///
        size_t getFileSize() const;

        ///
        /// STL API to get the raw file data in a vector.
        ///
        const std::vector<uint8_t>& data() const;

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
        void setEntryPoint(EA x);

        ///
        /// Gets the entry point of loaded file.
        ///
        EA getEntryPoint() const;

        ///
        /// If an invalid pair is passed in, the min and max will be set to an invalid state
        /// (gtirb::constants::BadAddress).
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
        void setRelocated();

        ///
        /// \return     True if the loaded image has been relocated.
        ///
        bool getRelocated() const;

        ///
        /// Sets data at the given address.
        ///
        /// \param  ea      The address to store the data.
        /// \param  x       The data to store (honoring Endianness).
        ///
        void setData(EA ea, uint8_t x);
        void setData(EA ea, uint16_t x);
        void setData(EA ea, uint32_t x);
        void setData(EA ea, uint64_t x);
        void setData(EA ea, gsl::span<uint8_t> x);

        ///
        /// Get data at the given address.
        ///
        /// Use the gtirb::utilities functions (i.e. ByteArray8To16) to translate this into 16, 32,
        /// or 64-bits.
        ///
        /// \param  x       The starting address for the data.
        /// \param  bytes   The number of bytes to read.
        ///
        std::vector<uint8_t> getData(EA x, size_t bytes) const;

        ///
        /// Get data at the given address until a termination condition is met.
        ///
        /// Use the gtirb::utilities functions (i.e. ByteArray8To16) to translate this into 16, 32,
        /// or 64-bits.
        ///
        /// \param  x       The starting address for the data.
        /// \param  terminate   A function that returns "true" when byte reading should terminate.
        ///
        std::vector<uint8_t> getData(EA x, std::function<bool(uint8_t)> terminate) const;

        ///
        /// Set the Global Offset Table EA.
        ///
        /// This is used for relocations expressed with "GOT".  It appears that it is the address of
        /// the symbol _GLOBAL_OFFSET_TABLE_.
        ///
        void setGlobalOffsetTableEA(EA x);

        ///
        /// This is used for relocations expressed with "GOT".  It appears that it is the address of
        /// the symbol _GLOBAL_OFFSET_TABLE_.
        ///
        EA getGlobalOffsetTableEA() const;

        ///
        ///
        ///
        void setContentSource(ContentSource x);

        ///
        ///
        ///
        ContentSource getContentSource() const;

    private:
        // Storage for the entire contents of the file.
        gtirb::ImageByteMap imageByteMap;

        boost::filesystem::path fileName;
    };
}
