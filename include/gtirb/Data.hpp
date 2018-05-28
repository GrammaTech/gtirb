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

        Data() = delete;

        Data(EA x) : ea(x)
        {
        }

        virtual Data::Type getType() const = 0;

        virtual ~Data() = default;

        EA getEA() const
        {
            return this->ea;
        }

    private:
        EA ea{0};
    };

    ///
    /// \class DataLabelMarker
    ///
    class GTIRB_GTIRB_EXPORT_API DataLabelMarker : public Data
    {
    public:
        DataLabelMarker(EA x) : Data(x)
        {
        }

        ~DataLabelMarker() override = default;

        Data::Type getType() const override
        {
            return Data::Type::LabelMarker;
        }
    };

    ///
    /// \class DataPLTReference
    ///
    class GTIRB_GTIRB_EXPORT_API DataPLTReference : public Data
    {
    public:
        DataPLTReference(EA x) : Data(x)
        {
        }

        ~DataPLTReference() override = default;

        Data::Type getType() const override
        {
            return Data::Type::PLTReference;
        }

        std::string function;
    };

    ///
    /// \class DataPointer
    ///
    class GTIRB_GTIRB_EXPORT_API DataPointer : public Data
    {
    public:
        DataPointer(EA x) : Data(x)
        {
        }

        ~DataPointer() override = default;

        Data::Type getType() const override
        {
            return Data::Type::Pointer;
        }

        EA content{0};
    };

    ///
    /// \class DataPointerDiff
    ///
    class GTIRB_GTIRB_EXPORT_API DataPointerDiff : public Data
    {
    public:
        DataPointerDiff(EA x) : Data(x)
        {
        }

        ~DataPointerDiff() override = default;

        Data::Type getType() const override
        {
            return Data::Type::PointerDiff;
        }

        EA symbol1{0};
        EA symbol2{0};
    };

    ///
    /// \class DataString
    ///
    class GTIRB_GTIRB_EXPORT_API DataString : public Data
    {
    public:
        DataString(EA x) : Data(x)
        {
        }

        ~DataString() override = default;

        Data::Type getType() const override
        {
            return Data::Type::String;
        }

        std::vector<uint8_t> getStringBytes(const Module& module) const;
        size_t size;
    };

    ///
    /// \class DataRawByte
    ///
    class GTIRB_GTIRB_EXPORT_API DataRawByte : public Data
    {
    public:
        DataRawByte(EA x) : Data(x)
        {
        }

        ~DataRawByte() override = default;

        Data::Type getType() const override
        {
            return Data::Type::RawByte;
        }

        uint8_t getByte(const Module& module) const;
    };
}
