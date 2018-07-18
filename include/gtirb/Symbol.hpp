#pragma once

#include <proto/Symbol.pb.h>
#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/NodeReference.hpp>

namespace gtirb
{
    ///
    /// \class Symbol
    /// \author John E. Farrier
    ///
    /// \note
    /// Renaming can lead to bugs, we probably want to separate symbols and
    /// abs-locs and have *both* in the IR.  Many current symbol fields would
    /// actually live in the abs-loc and be accessed from a symbol through the
    /// abs-loc.
    ///
    class GTIRB_GTIRB_EXPORT_API Symbol : public Node
    {
    public:
        ///
        /// \enum gtirb::Symbol::Type
        ///
        enum class Type : uint8_t
        {
            Undefined = proto::Type_Undefined,
            Normal = proto::Normal,
            DualCode = proto::DualCode,
            NoEA = proto::NoEA
        };

        ///
        /// \enum gtirb::Symbol::DeclarationKind
        ///
        enum class DeclarationKind : uint8_t
        {
            Undefined = proto::Decl_Undefined,
            Var = proto::Var,
            Func = proto::Func,
            Type = proto::Decl_Type,
            Param = proto::Param,
            MallocSite = proto::MallocSite,
            Result = proto::Result,
            Return = proto::Return,
            ParamTemp = proto::ParamTemp,
            Enumerator = proto::Enumerator,
            StringLiteral = proto::StringLiteral,
            Temporary = proto::Temporary,
            Uninit = proto::Uninit,
            Label = proto::Label,
            IndirectFunc = proto::IndirectFunc
        };

        ///
        /// \enum gtirb::Symbol::LinkType
        ///
        enum class LinkType : uint8_t
        {
            Undefined = proto::Link_Undefined,        /// undefined
            InitializedData = proto::InitializedData, /// initialized data
            BSS = proto::BSS,                         /// BSS
            Common = proto::Common,                   /// common
            WeakObject = proto::WeakObject,           /// weak object
            ReadOnly = proto::ReadOnly                /// read-only
        };

        ///
        /// \enum gtirb::Symbol::StorageKind
        ///
        enum class StorageKind : uint8_t
        {
            Undefined = proto::Storage_Undefined,
            Normal = proto::Storage_Normal,
            Static = proto::Storage_Static,
            Extern = proto::Storage_Extern
        };

        ///
        /// Default constructor.
        ///
        Symbol() = default;

        ///
        /// This constructor sets the Effective Address on construction.
        ///
        Symbol(EA x);
        Symbol(EA x, std::string name);

        ///
        /// Copy constructor. Assigns a new UUID to the copy.
        ///
        explicit Symbol(const Symbol& other) = default;

        ///
        /// Move constructor
        ///
        Symbol(Symbol&&) = default;

        ///
        /// Move assignment
        ///
        Symbol& operator=(Symbol&&) = default;

        ///
        /// Defaulted trivial destructor.
        ///
        ~Symbol() override = default;

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

        using MessageType = proto::Symbol;
        void toProtobuf(MessageType* message) const;
        void fromProtobuf(const MessageType& message);

    private:
        gtirb::EA ea{};
        std::string name;

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

    class SymbolReference : public NodeReference<Symbol>
    {
    public:
        using NodeReference::NodeReference;
    };
}
