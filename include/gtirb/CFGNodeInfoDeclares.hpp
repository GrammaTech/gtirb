#pragma once

#include <gtirb/CFGNodeInfo.hpp>

namespace gtirb
{
    ///
    /// \class CFGNodeInfoEntry
    /// \author John E. Farrier
    ///
    /// \todo Remove.  This may will eventually become something we
    /// store in an external table.
    ///
    class GTIRB_GTIRB_EXPORT_API CFGNodeInfoDeclares : public CFGNodeInfo
    {
    public:
        ///
        /// Defaulted trivial destructor.
        ///
        ~CFGNodeInfoDeclares() override = default;
    };
}
