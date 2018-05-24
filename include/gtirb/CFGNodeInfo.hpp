#pragma once

#include <boost/serialization/weak_ptr.hpp>
#include <cstdint>
#include <gtirb/Node.hpp>
#include <gtirb/Symbol.hpp>

namespace gtirb
{
    class Symbol;

    ///
    /// \class CFGNodeInfo
    /// \author John E. Farrier
    ///
    /// A base class for Node Specific Information (NSI) for a CFGNode.
    ///
    class GTIRB_GTIRB_EXPORT_API CFGNodeInfo : public Node
    {
    public:
        ///
        /// Default Constructor.
        ///
        CFGNodeInfo() = default;

        ///
        /// Defaulted trivial destructor.
        ///
        ~CFGNodeInfo() override = default;

        ///
        /// Symbols are owned by the IR->Module->Symbols
        ///
        /// \param  x   A non-owned pointer to a symbol to associate with this node.
        ///
        void setProcedureNameSymbol(gtirb::Symbol* x);

        ///
        /// The symbol associated with this Node, or nullptr.
        ///
        gtirb::Symbol* getProcedureNameSymbol() const;

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/)
        {
            ar& boost::serialization::base_object<Node>(*this);
            ar& procedureNameSymbol;
        }

    private:
        Symbol* procedureNameSymbol{nullptr};
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::CFGNodeInfo);
