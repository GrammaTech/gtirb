#pragma once

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
    /// \todo Remove.  I don't think we will have anything to store here.
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

    private:
        Symbol* procedureNameSymbol{nullptr};
    };
}
