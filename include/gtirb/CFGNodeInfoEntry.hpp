#pragma once

#include <gtirb/CFGNodeInfo.hpp>

namespace gtirb
{
    ///
    /// \class CFGNodeInfoEntry
    /// \author John E. Farrier
    ///
    class GTIRB_GTIRB_EXPORT_API CFGNodeInfoEntry final : public CFGNodeInfo
    {
    public:
        ///
        /// Defaulted trivial destructor.
        ///
        virtual ~CFGNodeInfoEntry() = default;

    private:
    };
}
