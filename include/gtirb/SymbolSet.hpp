#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/Symbol.hpp>
#include <memory>
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
        SymbolSet() = default;

        ///
        /// Defaulted trivial destructor.
        ///
        ~SymbolSet() override = default;

        ///
        /// Return all symbols.
        ///
        /// \return     All symbols in the set.
        ///
        const std::vector<Symbol>& getSymbols() const;

        ///
        /// Search for symbols at the given EA.
        ///
        /// \param x    The EA to search for.
        /// \return     All symbols at the given EA.
        ///
        std::vector<const Symbol*> getSymbols(gtirb::EA x) const;

        ///
        /// Add a symbol to the set.
        ///
        /// \param s    The symbol to add.
        /// \return     A reference to the new symbol.
        ///
        Symbol& addSymbol(Symbol&& s);

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/)
        {
            ar& boost::serialization::base_object<Node>(*this);
            ar& contents;
        }

    private:
        std::vector<Symbol> contents;
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::SymbolSet);
