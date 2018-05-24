#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <set>

#if __cplusplus >= 201703L
#include <optional>
template <typename T>
using optional = std::optional<T>;
#else
#include <boost/optional.hpp>
template <typename T>
using optional = boost::optional<T>;
#endif

namespace gtirb
{
    class Symbol;

    ///
    /// \class Instruction
    /// \author John E. Farrier
    ///
    /// There's this question about how the instructions are actually
    /// represented.  Maybe we should try to be *general* across multiple
    /// instruction representations, even including the ability to have
    /// multiple "instructions" per actual machine code instruction.  What
    /// would be interface for a drop-in instruction representation look like?
    ///
    /// - It would need to be able to disassemble (assuming we give it a
    ///     starting point in a binary).
    ///
    /// - It would need to have an idea of operands so that we could hold
    ///     facts about individual operands such as their type, literal/ref,
    ///     stack offset, range, etc...
    ///
    /// Then the GT-IRB layer would sit on top.  It would provide the structure
    /// holding these instructions, and it would contain information about
    /// function boundaries, and stack offsets, and maybe types.
    ///
    class GTIRB_GTIRB_EXPORT_API Instruction : public Node
    {
    public:
        struct MovedLabel
        {
            int64_t offset1{0};
            int64_t offset2{0};
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
            optional<std::string> pltReferenceName;
            optional<EA> directCallDestination;
            optional<MovedLabel> movedLabel;
            bool isGlobalSymbol;
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
        void setIsFallthrough(bool x);
        bool getIsFallthrough() const;

        ///
        /// From WALA.
        /// Can this instruction thrown an exception?
        /// https://github.com/wala/WALA/wiki/Intermediate-Representation-(IR)
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
        void serialize(Archive& ar, const unsigned int /*version*/)
        {
            ar& boost::serialization::base_object<Node>(*this);
            ar& ea;
            ar& numberOfUses;
            ar& isFallthrough;
            ar& isPEI;
        }

    private:
        gtirb::EA ea;
        int64_t numberOfUses{0};
        bool isFallthrough{false};
        bool isPEI{false};
        std::vector<SymbolicOperand> symbolicOperands;
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::Instruction);
