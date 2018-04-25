#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>

namespace gtirb
{
    class Procedure;

    ///
    /// \class ProcedureSet
    /// \author John E. Farrier
    ///
    /// Storage for all gtirb::Procedure objects for a single gtirb::Module.
    /// This has gtirb::Procedure children.
    ///
    class GTIRB_GTIRB_EXPORT_API ProcedureSet : public Node
    {
    public:
        ///
        /// Default constructor.
        ///
        ProcedureSet();

        ///
        /// Defaulted trivial destructor.
        ///
        virtual ~ProcedureSet() = default;

        ///
        /// Get the symbol at the given EA.
        ///
        /// \param x    The EA of the gtirb::Procedure to get.
        /// \return     The Procedure at the given EA or nullptr.
        ///
        Procedure* getProcedure(gtirb::EA x) const;

        ///
        /// Get or create the symbol at the given EA.
        ///
        /// This is preferable to adding symbols manually as it ensures no duplicate symbols are
        /// created.
        ///
        /// \param x    The EA of the gtirb::Procedure to get (or create).
        /// \return     The Procedure at the given EA.
        ///
        Procedure* getOrCreateProcedure(gtirb::EA x);

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int)
        {
            ar& boost::serialization::base_object<Node>(*this);
        }
    };
} // namespace gtirb

BOOST_CLASS_EXPORT_KEY(gtirb::ProcedureSet);
