#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <memory>
#include <vector>

namespace gtirb
{
    class CFG;

    ///
    /// \class CFGSet
    /// \author John E. Farrier
    ///
    /// Storage for all gtirb::CFG objects for a single gtirb::Module.
    /// This has gtirb::CFG children.
    ///
    class GTIRB_GTIRB_EXPORT_API CFGSet : public Node
    {
    public:
        ///
        /// Defaulted trivial destructor.
        ///
        ~CFGSet() override = default;

        ///
        /// Get the CFG at the given EA.
        ///
        /// \param x    The EA of the gtirb::CFG to get.
        /// \return     The CFG at the given EA or nullptr.
        ///
        CFG* getCFG(gtirb::EA x) const;

        ///
        /// Get the CFG with the given Procedure Name
        ///
        /// \param x    The procedure name of the gtirb::CFG to get.
        /// \return     The CFG at the given EA or nullptr.
        ///
        /// \sa CFG::getProcedureName()
        ///
        CFG* getCFG(const std::string& x) const;

        ///
        /// Get or create the CFG at the given EA.
        ///
        /// This is preferable to adding CFGs manually as it ensures no duplicate CFGs are
        /// created.
        ///
        /// \param x    The EA of the gtirb::CFG to get (or create).
        /// \return     The CFG at the given EA.
        ///
        CFG* getOrCreateCFG(gtirb::EA x);

        const std::vector<std::shared_ptr<CFG>>& getCFGs() const;

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/);

    private:
        std::vector<std::shared_ptr<CFG>> contents;
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::CFGSet);
