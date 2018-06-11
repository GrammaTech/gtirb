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
        enum class Type
        {
            LabelMarker,
            PLTReference,
            Pointer,
            PointerDiff,
            String,
            RawByte
        };

        // Default constructor required for serialization.
        Data() = default;

        Data(EA x) : ea(x)
        {
        }

        virtual Data::Type getType() const = 0;

        virtual ~Data() = default;

        EA getEA() const
        {
            return this->ea;
        }

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/);

    private:
        EA ea{0};
    };

    ///
    /// \class DataLabelMarker
    ///
    class GTIRB_GTIRB_EXPORT_API DataLabelMarker : public Data
    {
    public:
        // Default constructor required for serialization;
        DataLabelMarker() = default;

        DataLabelMarker(EA x) : Data(x)
        {
        }

        ~DataLabelMarker() override = default;

        Data::Type getType() const override
        {
            return Data::Type::LabelMarker;
        }

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/);
    };

    ///
    /// \class DataPLTReference
    ///
    class GTIRB_GTIRB_EXPORT_API DataPLTReference : public Data
    {
    public:
        // Default constructor required for serialization;
        DataPLTReference() = default;

        DataPLTReference(EA x) : Data(x)
        {
        }

        ~DataPLTReference() override = default;

        Data::Type getType() const override
        {
            return Data::Type::PLTReference;
        }

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/);

        std::string function;
    };

    ///
    /// \class DataPointer
    ///
    class GTIRB_GTIRB_EXPORT_API DataPointer : public Data
    {
    public:
        // Default constructor required for serialization;
        DataPointer() = default;

        DataPointer(EA x) : Data(x)
        {
        }

        ~DataPointer() override = default;

        Data::Type getType() const override
        {
            return Data::Type::Pointer;
        }

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/);

        EA content{0};
    };

    ///
    /// \class DataPointerDiff
    ///
    class GTIRB_GTIRB_EXPORT_API DataPointerDiff : public Data
    {
    public:
        // Default constructor required for serialization;
        DataPointerDiff() = default;

        DataPointerDiff(EA x) : Data(x)
        {
        }

        ~DataPointerDiff() override = default;

        Data::Type getType() const override
        {
            return Data::Type::PointerDiff;
        }

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/);

        EA symbol1{0};
        EA symbol2{0};
    };

    ///
    /// \class DataString
    ///
    class GTIRB_GTIRB_EXPORT_API DataString : public Data
    {
    public:
        // Default constructor required for serialization;
        DataString() = default;

        DataString(EA x) : Data(x)
        {
        }

        ~DataString() override = default;

        Data::Type getType() const override
        {
            return Data::Type::String;
        }

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/);

        std::vector<uint8_t> getStringBytes(const Module& module) const;
        size_t size;
    };

    ///
    /// \class DataRawByte
    ///
    class GTIRB_GTIRB_EXPORT_API DataRawByte : public Data
    {
    public:
        // Default constructor required for serialization;
        DataRawByte() = default;

        DataRawByte(EA x) : Data(x)
        {
        }

        ~DataRawByte() override = default;

        Data::Type getType() const override
        {
            return Data::Type::RawByte;
        }

        uint8_t getByte(const Module& module) const;

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/);
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::Data);
BOOST_CLASS_EXPORT_KEY(gtirb::DataLabelMarker);
BOOST_CLASS_EXPORT_KEY(gtirb::DataPLTReference);
BOOST_CLASS_EXPORT_KEY(gtirb::DataPointer);
BOOST_CLASS_EXPORT_KEY(gtirb::DataPointerDiff);
BOOST_CLASS_EXPORT_KEY(gtirb::DataString);
BOOST_CLASS_EXPORT_KEY(gtirb::DataRawByte);
