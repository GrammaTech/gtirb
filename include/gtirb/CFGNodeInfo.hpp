#pragma once

#include <cstdint>
#include <gtirb/Node.hpp>
#include <gtirb/Symbol.hpp>
#include <boost/serialization/weak_ptr.hpp>

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
        CFGNodeInfo();

        ///
        /// Defaulted trivial destructor.
        ///
        virtual ~CFGNodeInfo() = default;

        ///
        /// Symbols are owned by the IR->Module->Symbols
        ///
        /// \param  x   A non-owned pointer to a symbol to assicate with this node.
        ///
        /// \throws std::bad_weak_ptr if 'x' is not owned by the GTIR (a shared_ptr).
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
        void serialize(Archive& ar, const unsigned int version)
        {
            ar& boost::serialization::base_object<Node>(*this);
            ar& procedureNameSymbol;
        }

    private:
        std::weak_ptr<gtirb::Symbol> procedureNameSymbol;
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::CFGNodeInfo);
