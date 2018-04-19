#pragma once

#include <cstdint>
#include <gtirb/Node.hpp>

namespace gtirb
{
    class EA;

    ///
    /// \class CFG
    /// \author John E. Farrier
    ///
    class GTIRB_GTIRB_EXPORT_API CFG final : public Node
    {
    public:
        ///
        /// Default Constructor.
        ///
        CFG();

        ///
        /// Defaulted trivial destructor.
        ///
        virtual ~CFG() = default;

        //Procedure* getOrCreateProcedure();
        //CFGAttribute* getOrCreateCFGAttribute();

    private:
        //std::weak_ptr<CFGNode> first;
        //std::weak_ptr<CFGNode> last;
        //SRPSet_T srpSet;
    };
}
