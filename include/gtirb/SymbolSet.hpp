#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>

namespace gtirb
{
    class Symbol;

    ///
    /// \class SymbolSet
    /// \author John E. Farrier
    ///
    /// Storage for all gtirb::Symbol objects for a single gtirb::Module.
    /// This has gtirb::Symbol children.
    ///
    class GTIRB_GTIRB_EXPORT_API SymbolSet : public Node
    {
    public:
        ///
        /// Default constructor.
        ///
        SymbolSet();

        ///
        /// Defaulted trivial destructor.
        ///
        virtual ~SymbolSet() = default;

        ///
        /// Get the symbol at the given EA.
        ///
        /// \param x    The EA of the gtirb::Symbol to get.
        /// \return     The Symbol at the given EA or nullptr.
        ///
        Symbol* getSymbol(gtirb::EA x) const;

        ///
        /// Get or create the symbol at the given EA.
        ///
        /// This is preferable to adding symbols manually as it ensures no duplicate symbols are created.
        ///
        /// \param x    The EA of the gtirb::Symbol to get (or create).
        /// \return     The Symbol at the given EA.  
        ///
        Symbol* getOrCreateSymbol(gtirb::EA x);

        template <class Archive>
        void serialize(Archive& ar, const unsigned int)
        {
            ar& boost::serialization::base_object<Node>(*this);
        }
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::SymbolSet);
