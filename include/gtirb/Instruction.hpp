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
        /// Defaulted trivial destructor.
        ///
        ~Instruction() override = default;

        void setEA(gtirb::EA x);
        gtirb::EA getEA() const;

        ///
        /// From WALA.
        /// https://github.com/wala/WALA/wiki/Intermediate-Representation-(IR)
        ///
        /// \todo Remove.
        ///
        void setIsFallthrough(bool x);
        bool getIsFallthrough() const;

        ///
        /// From WALA.
        /// Can this instruction thrown an exception?
        /// https://github.com/wala/WALA/wiki/Intermediate-Representation-(IR)
        ///
        /// \todo Remove.
        ///
        void setIsPEI(bool x);
        bool getIsPEI() const;

        void setNumberOfUses(int64_t x);
        int64_t getNumberOfUses() const;

        using MessageType = proto::Instruction;
        void toProtobuf(MessageType* message) const;
        void fromProtobuf(const MessageType& message);

    private:
        gtirb::EA ea;
        int64_t numberOfUses{0};
        bool isFallthrough{false};
        bool isPEI{false};
    };
}
