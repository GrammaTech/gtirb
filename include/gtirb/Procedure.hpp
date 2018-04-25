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
    class GTIRB_GTIRB_EXPORT_API Procedure : public Node
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

        void setEA(gtirb::EA x);
        gtirb::EA getEA() const;

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

        ///
        /// Gets an instruction associated with this procedure.
        ///
        gtirb::Instruction* getOrCreateInstruction();

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int version)
        {
            ar& boost::serialization::base_object<Node>(*this);
            ar& this->ea;
            ar& this->pltEntries;
        }

    private:
        gtirb::EA ea;
        std::set<gtirb::EA> pltEntries;
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::Procedure);
