#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <set>

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
    /// Symbolic information is an expression which may combine
    /// references as symbols of type gtirb::Symbol with simple
    /// mathematical expressions including numeric constants.
    ///
    class GTIRB_GTIRB_EXPORT_API Instruction : public Node
    {
    public:
        struct MovedLabel
        {
            int64_t offset1{0};
            int64_t offset2{0};

            template <class Archive>
            void serialize(Archive& ar, const unsigned int /*version*/);
        };

        enum class SymbolicKind
        {
            PLTReference,
            DirectCall,
            MovedLabel,
            GlobalSymbol,
            None
        };

        /// \class SymbolicOperand
        ///
        /// \todo This lines up with the datalog-disassembler/pretty-printer,
        /// is it the right design for gt-irb?
        ///
        /// Typically only one of these fields will be set. Maybe this should
        /// be a union/variant instead?
        struct SymbolicOperand
        {
            SymbolicKind kind;
            std::string pltReferenceName;
            EA directCallDestination;
            MovedLabel movedLabel;

            template <class Archive>
            void serialize(Archive& ar, const unsigned int /*version*/);
        };

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

        std::vector<SymbolicOperand>& getSymbolicOperands();
        const std::vector<SymbolicOperand>& getSymbolicOperands() const;

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/);

    private:
        gtirb::EA ea;
        int64_t numberOfUses{0};
        bool isFallthrough{false};
        bool isPEI{false};
        std::vector<SymbolicOperand> symbolicOperands;
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::Instruction);
