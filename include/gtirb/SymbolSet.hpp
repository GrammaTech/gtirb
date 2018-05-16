#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <vector>

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
        ~SymbolSet() override = default;

        ///
        /// Recursively find all symbols.
        ///
        /// \return     All symbols in the set.
        ///
        std::vector<Symbol*> getSymbols() const;

        ///
        /// Recursively search for symbols at the given EA.
        ///
        /// \param x    The EA to search for.
        /// \return     All symbols at the given EA.
        ///
        std::vector<Symbol*> getSymbols(gtirb::EA x) const;

        ///
        /// Add a symbol to the set.
        ///
        /// \param s    The symbol to add.
        /// \return     A non-owning pointer to the new symbol.
        ///
        Symbol* addSymbol(std::unique_ptr<Symbol>&& s);

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/)
        {
            ar& boost::serialization::base_object<Node>(*this);
        }
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::SymbolSet);
