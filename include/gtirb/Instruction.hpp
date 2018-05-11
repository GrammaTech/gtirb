#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <set>

namespace gtirb
{
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
        ///
        /// Default Constructor.
        ///
        Instruction();

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
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::Instruction);
