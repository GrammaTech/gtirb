#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <set>

namespace gtirb
{
    class Instruction;
    
    ///
    /// \class Procedure
    /// \author John E. Farrier
    ///
    class GTIRB_GTIRB_EXPORT_API Procedure final : public Node
    {
    public:
        ///
        /// Default Constructor.
        ///
        Procedure();

        ///
        /// Defaulted trivial destructor.
        ///
        virtual ~Procedure() = default;

        ///
        /// Procedure Linkage Table.
        /// These entries are basically the "thunks" for calls to things in shared libraries.
        ///
        std::set<gtirb::EA>* getPLTEntries();

        ///
        /// Procedure Linkage Table.
        /// These entries are basically the "thunks" for calls to things in shared libraries.
        ///
        const std::set<gtirb::EA>* const getPLTEntries() const;

        gtirb::Instruction* getOrCreateInstruction();

    private:
        std::set<gtirb::EA> pltEntries;
    };
}
