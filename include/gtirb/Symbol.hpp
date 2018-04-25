#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>

namespace gtirb
{
    ///
    /// \class Symbol
    /// \author John E. Farrier
    ///
    class GTIRB_GTIRB_EXPORT_API Symbol : public Node
    {
    public:
        ///
        /// \enum gtirb::Symbol::Type
        ///
        enum class Type : uint8_t
        {
            Undefined,
            Normal,
            DualCode,
            NoEA
        };

        ///
        /// \enum gtirb::Symbol::DeclarationKind
        ///
        enum class DeclarationKind : uint8_t
        {
            Undefined,
            Var,
            Func,
            Type,
            Param,
            MallocSite,
            Result,
            Return,
            ParamTemp,
            Enumerator,
            StringLiteral,
            Temporary,
            Uninit,
            Label,
            IndirectFunc
        };

        ///
        /// \enum gtirb::Symbol::LinkType
        ///
        enum class LinkType : uint8_t
        {
            Undefined,       /// undefined
            InitializedData, /// initialized data
            BSS,             /// BSS
            Common,          /// common
            WeakObject,      /// weak object
            ReadOnly         /// read-only
        };

        ///
        /// \enum gtirb::Symbol::StorageKind
        ///
        enum class StorageKind : uint8_t
        {
            Undefined,
            Normal,
            Static,
            Extern
        };

        ///
        /// Default constructor.
        ///
        Symbol();

        ///
        /// This constructor sets the Effective Address on construction.
        ///
        Symbol(EA x);

        ///
        /// Defaulted trivial destructor.
        ///
        virtual ~Symbol() = default;

        void setEA(gtirb::EA x);
        gtirb::EA getEA() const;

        void setName(std::string x);
        std::string getName() const;

        void setType(gtirb::Symbol::Type x);
        gtirb::Symbol::Type getType() const;

        void setOffset(int64_t x);
        int64_t getOffset();

        void setElementSize(int64_t x);
        int64_t getElementSize() const;

        /// The total number of bits for this symbol.
        void setBitSize(int64_t x);
        int64_t getBitSize() const;

        void setIsFormal(bool x);
        bool getIsFormal() const;

        void setEnableForceName(bool x);
        bool getEnableForceName() const;

        void setEnableGapSize(bool x);
        bool getEnableGapSize() const;

        void setIsNameOnly(bool x);
        bool getIsNameOnly() const;

        void setDeclarationKind(gtirb::Symbol::DeclarationKind x);
        gtirb::Symbol::DeclarationKind getDeclarationKind() const;

        void setLinkType(Symbol::LinkType x);
        gtirb::Symbol::LinkType getLinkType() const;

        void setStorageKind(Symbol::StorageKind x);
        gtirb::Symbol::StorageKind getStorageKind() const;

        void setIsGlobal(bool x);
        bool getIsGlobal() const;

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int)
        {
            ar& boost::serialization::base_object<Node>(*this);

            ar & this->name;
            ar & this->ea;
            ar & this->offset;
            ar & this->elementSize;
            ar & this->bitSize;
            ar & this->type;
            ar & this->declarationKind;
            ar & this->linkType;
            ar & this->storageKind;
            ar & this->enableForceName;
            ar & this->enableGapSize;
            ar & this->isFormal;
            ar & this->isNameOnly;
            ar & this->isGlobal;
        }

    private:
        std::string name;
        gtirb::EA ea{};

        int64_t offset{0};
        int64_t elementSize{0};
        int64_t bitSize{0};

        gtirb::Symbol::Type type{};
        gtirb::Symbol::DeclarationKind declarationKind{};
        gtirb::Symbol::LinkType linkType{};
        gtirb::Symbol::StorageKind storageKind{};

        bool enableForceName{false};
        bool enableGapSize{false};
        bool isFormal{false};
        bool isNameOnly{false};
        bool isGlobal{false};
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::Symbol);
