#pragma once

#include <proto/Symbol.pb.h>
#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/NodeReference.hpp>

namespace gtirb
{
    class Data;
    class Instruction;

    ///
    /// \class Symbol
    /// \author John E. Farrier
    ///
    class GTIRB_GTIRB_EXPORT_API Symbol : public Node
    {
    public:
        ///
        ///
        /// \enum gtirb::Symbol::StorageKind
        ///
        enum class StorageKind : uint8_t
        {
            Undefined = proto::Storage_Undefined,
            Normal = proto::Storage_Normal,
            Static = proto::Storage_Static,
            Extern = proto::Storage_Extern,
            Local = proto::Storage_Local
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

        ///
        /// Set the Data object to which this symbol refers.
        ///
        void setReferent(const Data& data);

        ///
        /// Set the Instruction object to which this symbol refers.
        ///
        void setReferent(const Instruction& instruction);

        ///
        /// Get the Data object to which this symbol refers.
        ///
        NodeReference<Data> getDataReferent() const;

        ///
        /// Get the Instruction object to which this symbol refers.
        ///
        NodeReference<Instruction> getCodeReferent() const;

        void setStorageKind(Symbol::StorageKind x);
        gtirb::Symbol::StorageKind getStorageKind() const;

        using MessageType = proto::Symbol;
        void toProtobuf(MessageType* message) const;
        void fromProtobuf(const MessageType& message);

    private:
        gtirb::EA ea{};
        std::string name;
        NodeReference<Data> dataReferent;
        NodeReference<Instruction> codeReferent;

        uint64_t size{0};
        gtirb::Symbol::StorageKind storageKind{};
    };

    class SymbolReference : public NodeReference<Symbol>
    {
    public:
        using NodeReference::NodeReference;
    };
}
