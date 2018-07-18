#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Instruction.hpp>
#include <gtirb/Node.hpp>
#include <set>

namespace proto
{
    class Procedure;
}
namespace gtirb
{
    class Instruction;

    ///
    /// \class Procedure
    /// \author John E. Farrier
    ///
    class GTIRB_GTIRB_EXPORT_API Procedure : public Node
    {
    public:
        ///
        /// Default Constructor.
        ///
        Procedure() = default;

        ///
        /// Copy constructor. Assigns a new UUID to the copy.
        ///
        explicit Procedure(const Procedure& other) = default;

        ///
        /// Move constructor
        ///
        Procedure(Procedure&&) = default;

        ///
        /// Move assignment
        ///
        Procedure& operator=(Procedure&&) = default;

        ///
        /// Defaulted trivial destructor.
        ///
        ~Procedure() override = default;

        void setEA(gtirb::EA x);
        gtirb::EA getEA() const;

        ///
        /// Procedure Linkage Table.
        /// These entries are basically the "thunks" for calls to things in shared libraries.
        ///
        std::set<gtirb::EA>& getPLTEntries();

        ///
        /// Procedure Linkage Table.
        /// These entries are basically the "thunks" for calls to things in shared libraries.
        ///
        const std::set<gtirb::EA>& getPLTEntries() const;

        ///
        /// Add a new instruction to this procedure.
        ///
        Instruction* createInstruction();

        using MessageType = proto::Procedure;
        void toProtobuf(MessageType* message) const;
        void fromProtobuf(const MessageType& message);

        /// \todo Several pieces of information probably need added to gtirb::Procedure.  This
        /// includes a pointer to its name symbol, a symbol-to-symbol map, a save-to-restore-symbol
        /// map, an ea-to-RegionHeap map, a frame base EA, a return address EA, the size of the Save
        /// Register slot, and the offset of the Save Register Slot.

    private:
        gtirb::EA ea;
        std::set<EA> pltEntries;
        std::vector<Instruction> instructions;
    };
}
