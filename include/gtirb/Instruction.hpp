#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <set>

namespace proto
{
    class Instruction;
}

namespace gtirb
{
    class Symbol;

    ///
    /// \class Instruction
    /// \author John E. Farrier
    ///
    /// Instructions have two fields, a pointer to the raw bytes of
    /// the instruction (including any instruction prefix) and a list
    /// of pairs of operand indices and associated pointers to
    /// symbolic information for that operand.
    ///
    /// Byte pointers are offsets into the ImageByteMap.
    ///
    class GTIRB_GTIRB_EXPORT_API Instruction : public Node
    {
    public:
        ///
        /// Default Constructor.
        ///
        Instruction() = default;
        Instruction(EA ea);

        ///
        /// Copy constructor. Assigns a new UUID to the copy.
        ///
        explicit Instruction(const Instruction& other) = default;

        ///
        /// Move constructor
        ///
        Instruction(Instruction&&) = default;

        ///
        /// Move assignment
        ///
        Instruction& operator=(Instruction&&) = default;

        ///
        /// Defaulted trivial destructor.
        ///
        // virtual ~Instruction() override = default;

        void setEA(gtirb::EA x);
        gtirb::EA getEA() const;

        using MessageType = proto::Instruction;
        void toProtobuf(MessageType* message) const;
        void fromProtobuf(const MessageType& message);

    private:
        gtirb::EA ea;
    };
}
