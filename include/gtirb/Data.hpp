#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>

namespace gtirb
{
    class Module;

    ///
    /// \class Data
    ///
    /// Represents a data object, possibly symbolic. Does not directly store
    /// the data bytes, which are kept in the ImageByteMap.
    ///
    /// \note
    /// This seems very similar to the Instruction class since each
    /// piece of data basically holds a pointer to bytes and some
    /// amount of symoblic information which may be comprised as a
    /// simple mathematical expression combining symobls.  The only
    /// addition is that data should have a `size` field (any other
    /// information like types should be stored in external tables).
    ///
    /// Perhaps data and instruction should share a base class which
    /// provides the byte-pointer and symbolic expression support?
    ///
    class GTIRB_GTIRB_EXPORT_API Data : public Node
    {
    public:
        // Default constructor required for serialization.
        Data() = default;

        Data(EA ea_, uint64_t size_) : ea(ea_), size(size_)
        {
        }

        EA getEA() const;

        uint64_t getSize() const;

        std::vector<uint8_t> getBytes(const Module& module) const;

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/);

    private:
        EA ea{0};
        uint64_t size{0};
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::Data);
